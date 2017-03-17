(defproject org.clojars.mike706574/misplaced-villages "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.495"]]
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler {:optimizations :none
                                   :pretty-print true}}]}
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/tools.namespace "0.2.11"]]}})
