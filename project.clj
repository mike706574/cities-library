(defproject mike/misplaced-villages "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.taoensso/timbre "4.8.0"]
                 [dire "0.5.4"]]
  :min-lein-version "2.0.0"
  :source-paths ["src/clj"]
  :test-paths ["test/clj"]
  :plugins [[lein-environ "1.1.0"]]
  :uberjar-name "misplaced-villages-standalone.jar"
  :profiles {:uberjar {:aot :all
                       :main misplaced-villages.main}
             :dev {:source-paths ["dev"]
                   :target-path "target/dev"
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/tools.namespace "0.2.11"]
                                  [criterium "0.4.4"]
                                  [clj-http "3.4.1"]
                                  [org.clojure/data.json "0.2.6"]]}}
  :repl-options {:init-ns user})
