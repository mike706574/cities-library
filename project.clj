(defproject mike/misplaced-villages "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/math.combinatorics "0.1.4"]]
  :min-lein-version "2.0.0"
  :source-paths ["src/clj"]
  :test-paths ["test/clj"]
  :profiles {:dev {:source-paths ["dev"]
                   :target-path "target/dev"
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/tools.namespace "0.2.11"]
                                  [criterium "0.4.4"]]}}
  :repl-options {:init-ns user})
