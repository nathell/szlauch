(ns szlauch.core
  "Pipeline gluing JMorfeusz (morphological analysis) and clj-concraft (POS tagging).
   Takes Polish text, runs it through Morfeusz, converts the result to a concraft DAG,
   then disambiguates with the concraft model."
  (:require [clojure.string :as str]
            [concraft.dag :as dag]
            [concraft.model :as model]
            [concraft.polish :as polish]
            [concraft.format :as fmt])
  (:import [pl.sgjp.jmorfeusz Morfeusz MorphInterpretation WhitespaceHandling]))

(defn create-morfeusz
  "Create a Morfeusz analyzer instance with KEEP_WHITESPACES mode."
  []
  (let [m (Morfeusz/createInstance)]
    (.setWhitespaceHandling m WhitespaceHandling/KEEP_WHITESPACES)
    m))

(defn load-concraft-model
  "Load a concraft model from a gzip-compressed binary file."
  [path]
  (model/load-model path))

(defn morfeusz-analyze
  "Run Morfeusz morphological analysis on text. Returns a seq of MorphInterpretation."
  [^Morfeusz morfeusz ^String text]
  (.analyze morfeusz text))

(defn- strip-lemma-annotation
  "Strip the :foo annotation from a Morfeusz lemma, e.g. \"prawo:S\" → \"prawo\"."
  [lemma]
  (let [idx (str/index-of lemma ":")]
    (if idx (subs lemma 0 idx) lemma)))

(defn- explode-tag
  "Explode a Morfeusz tag with dot-separated alternatives into all combinations.
   e.g. \"subst:sg:gen.acc:m1\" → [\"subst:sg:gen:m1\" \"subst:sg:acc:m1\"]"
  [tag]
  (let [parts (str/split tag #":")
        alternatives (map #(str/split % #"\.") parts)]
    (reduce (fn [acc alts]
              (for [prefix acc
                    alt alts]
                (str prefix ":" alt)))
            (map str (first alternatives))
            (rest alternatives))))

(defn interps->dag
  "Convert a list of MorphInterpretations from Morfeusz into a concraft DAG.
   Filters out whitespace tokens, resolves tag IDs to tag strings, and explodes
   dot-separated tag alternatives into separate interpretations.
   Returns {:dag dag, :space-before #{node-ids}} where space-before contains
   the start nodes of edges that are preceded by whitespace."
  [^Morfeusz morfeusz interps]
  (let [;; Build node-merge map from whitespace edges: endNode → startNode
        ;; This collapses the gap left by removing whitespace edges.
        ws-merge (into {} (keep (fn [^MorphInterpretation interp]
                                  (when (.isWhitespace interp)
                                    [(.getEndNode interp) (.getStartNode interp)])))
                        interps)
        ;; Resolve merge chains transitively
        resolve (fn [n] (loop [n n] (if-let [m (ws-merge n)] (recur m) n)))
        ;; Track which original start-nodes had whitespace before them
        space-before-orig (set (keys ws-merge))
        ;; Filter out whitespace
        non-ws (remove #(.isWhitespace ^MorphInterpretation %) interps)
        ;; Apply merges, collect all resulting node IDs, renumber contiguously
        merged-nodes (into (sorted-set)
                           (mapcat (fn [^MorphInterpretation interp]
                                     [(resolve (.getStartNode interp))
                                      (resolve (.getEndNode interp))]))
                           non-ws)
        renum (into {} (map-indexed (fn [i n] [n i])) merged-nodes)
        remap (fn [n] (renum (resolve n)))
        ;; Track space-before using renumbered node IDs
        space-before (into #{} (keep (fn [^MorphInterpretation interp]
                                       (when (contains? space-before-orig (.getStartNode interp))
                                         (remap (.getStartNode interp)))))
                           non-ws)
        ;; Group by (remapped startNode, endNode, orth) to form edges
        ;; Use LinkedHashMap to preserve insertion order
        lhm (java.util.LinkedHashMap.)
        _ (doseq [^MorphInterpretation interp non-ws]
            (let [k [(remap (.getStartNode interp)) (remap (.getEndNode interp)) (.getOrth interp)]]
              (.put lhm k (conj (or (.get lhm k) []) interp))))
        ;; Build edge data for dag/from-edges
        edges (mapv (fn [[[start end orth] group]]
                      (let [known (not (some #(.isIgn ^MorphInterpretation %) group))
                            tags (into {}
                                       (mapcat (fn [^MorphInterpretation interp]
                                                 (let [tag-str (.getTag morfeusz (.getTagId interp))
                                                       exploded (explode-tag tag-str)]
                                                   (map (fn [t]
                                                          [{:base (strip-lemma-annotation (.getLemma interp))
                                                            :tag t
                                                            :commonness nil
                                                            :qualifier nil
                                                            :meta-info nil
                                                            :eos false}
                                                           0.0])
                                                        exploded))))
                                       group)]
                        {:tail start
                         :head end
                         :label {:word {:orth orth
                                        :known known
                                        :word-info nil}
                                 :tags tags}}))
                    (into [] lhm))]
    {:dag (dag/from-edges edges)
     :space-before space-before}))

(defn analyze
  "Full pipeline: Polish text → morphological analysis → POS disambiguation.
   Returns {:anno-sents [...], :space-before #{node-ids}}."
  [morfeusz concraft-model text]
  (let [interps (morfeusz-analyze morfeusz text)
        {:keys [dag space-before]} (interps->dag morfeusz interps)]
    (spit "/tmp/concraft-input.txt"
           (->> (clojure.string/split-lines (fmt/show-input-sent dag))
                (remove clojure.string/blank?)
                (map (fn [line]
                       (let [cols (clojure.string/split line #"\t" -1)
                             padded (concat cols (repeat (max 0 (- 11 (count cols))) ""))]
                         (clojure.string/join "\t" (take 11 padded)))))
                (clojure.string/join "\n")
                (#(str % "\n"))))
    {:anno-sents (polish/anno-all concraft-model dag)
     :space-before space-before}))

(defn- pick-best-disamb
  "Given a seq of disambiguated interps, pick one. Prefer all-lowercase :base, otherwise first."
  [interps]
  (or (first (filter #(= (:base %) (str/lower-case (:base %))) interps))
      (first interps)))

(defn disamb-tokens
  "Extract only the disambiguated tokens from annotated sentences.
   Returns one map per edge with :orth, :base, :tag, :space-before for the best
   disambiguated interpretation. space-before is a set of Morfeusz start-node IDs
   that are preceded by whitespace."
  [{:keys [anno-sents space-before]}]
  (for [{:keys [dag disambs]} anno-sents
        eid (dag/dag-edges dag)
        :let [{:keys [word tags]} (dag/edge-label dag eid)
              tail-node (dag/begins-with dag eid)
              edge-disambs (get disambs eid {})
              selected (for [[interp _prob] tags
                             ;; Disamb runs on EOS-stripped dag, so look up with :eos false
                             :let [lookup-interp (assoc interp :eos false)]
                             :when (get edge-disambs lookup-interp false)]
                         {:orth (:orth word)
                          :base (:base interp)
                          :tag (:tag interp)
                          :space-before (contains? space-before tail-node)})
              best (pick-best-disamb selected)]
        :when best]
    best))

(defn analyze-and-format
  "Full pipeline with formatted text output."
  [morfeusz concraft-model text]
  (let [{:keys [anno-sents]} (analyze morfeusz concraft-model text)]
    (polish/format-annotated-sents anno-sents)))

(defn simple-analyze
  "Runs the pipeline and formats the output Poliqarp-style."
  [morfeusz concraft-model text]
  (str/join ""
            (map #(format "%s%s [%s:%s]" (if (:space-before %) " " "") (:orth %) (:base %) (:tag %))
                 (disamb-tokens (analyze morfeusz concraft-model text)))))

(defn lemmatize
  [morfeusz concraft-model text]
  (let [toks (->> text
                  (analyze morfeusz concraft-model)
                  (disamb-tokens))
        filtered (concat (take 1 toks)
                         (filter #(or (:space-before %) (= (:tag %) "interp"))
                                 (next toks)))]
    (->> filtered
         (map #(let [base (if (= (:base %) "none")
                            (:orth %)
                            (:base %))]
                 (if (:space-before %) (str " " base) base)))
         (str/join ""))))

(comment
  ;; Usage example:
  (def morfeusz (create-morfeusz))
  (def model (load-concraft-model "concraft-pl-model-SGJP-20220221.gz"))

  ;; Analyze Polish text
  (println (simple-analyze morfeusz model "Ala ma kota."))

  ;; Or get structured results
  (def results (analyze morfeusz model "Mam próbkę analizy morfologicznej.")))
