(ns pipeline.core
  (:require [clojure.string :as str]
            [loom.graph :as g]
            [loom.alg :as alg]
            [loom.io :as io])
  (:import [pl.sgjp.jmorfeusz Morfeusz]
           [pl.sgjp.jmorfeusz.segrules SegrulesFSA SegrulesState]))

(def m (Morfeusz/createInstance))

(defn invoke-private-method [obj fn-name-string & args]
  (let [m (first (filter (fn [x] (.. x getName (equals fn-name-string)))
                   (.. obj getClass getDeclaredMethods)))]
    (. m (setAccessible true))
    (. m (invoke obj args))))

(defn private-field [obj fn-name-string]
  (let [m (.. obj getClass (getDeclaredField fn-name-string))]
    (. m (setAccessible true))
    (. m (get obj))))

(defn segrules-transitions [fsa offset]
  (let [data (private-field fsa "data")
        _ (.position data offset)
        flags (bit-and (.get data) 255)
        transitions (bit-and (.get data) 255)]
    (map (fn [i]
           (let [segnum (bit-and (.get data) 255)
                 shift (bit-and (.get data) 255)
                 target (bit-and (.getShort data) 65535)]
             {:src offset
              :shift (not (zero? shift))
              :segnum segnum
              :dest target}))
         (range transitions))))

(defn segrules-state-info [fsa offset]
  (let [data (private-field fsa "data")
        _ (.position data offset)
        flags (bit-and (.get data) 255)]
    {:offset offset, :accepting (not= 0 (bit-and flags 1)), :weak (not= 0 (bit-and flags 2))}))

(defn label-node [{:keys [offset accepting weak]}]
  (if (and (not accepting) (not weak))
    (str offset)
    (str offset " [" (str/join ", " (remove nil? [(when accepting "A")
                                                 (when weak "W")]))
         "]")))

(defn label-edge [{:keys [shift segnum]}]
  (str segnum ", " shift))

(defn dot-str [g]
  (io/dot-str g
              :node-label #(label-node (segrules-state-info g %))
              :edge-label (fn [n1 n2]
                            (->> (g/out-edges g n1)
                                 (filter #(= (:dest %) n2))
                                 first
                                 label-edge))))

(extend-protocol g/Graph
  SegrulesFSA
  (nodes [g]
    (alg/bf-traverse g 0))
  (edges [g]
    (for [n1 (g/nodes g)
          e (g/out-edges g n1)]
      [n1 (:dest e)]))
  (has-node? [g node]
    (contains? (set (g/nodes g)) node))
  (has-edge? [g n1 n2]
    (contains? (set (g/successors* g n1)) n2))
  (successors* [g node]
    (map :dest (g/out-edges g node)))
  (out-degree [g node]
    (count (g/out-edges g node)))
  (out-edges [g node]
    (segrules-transitions g node)))

(extend-protocol g/Digraph
  SegrulesFSA
  (predecessors* [g node] nil)
  (in-degree [g node] nil)
  (in-edges [g node] nil)
  (transpose [g] nil))

(def fsa (private-field m "segrulesFSA"))

(count (g/nodes fsa))

(spit "/tmp/a.dot" (dot-str fsa))
