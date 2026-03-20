# szlauch

An experimental morphosyntactic tagging pipeline for Polish, putting together [JMorfeusz](https://github.com/nathell/jmorfeusz) and [clj-concraft](https://github.com/nathell/clj-concraft).

## Try it out

1. Set up access token in your `~/.m2/settings.xml` as per [this guide](https://stackoverflow.com/questions/58438367/how-to-access-maven-dependecy-from-github-package-registry-beta/58453517#58453517)

2. Get a Concraft model (`concraft-pl-model-SGJP-20220221.gz`) from [here](https://zil.ipipan.waw.pl/Concraft)

3. Launch a Clojure REPL (`clj`)

4. Evaluate the code samples at the end of `core.clj`
