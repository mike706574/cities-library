(defproject fun.mike/cities "0.0.1-SNAPSHOT"
  :description "Lost Cities in Clojure."
  :url "https://github.com/mike706574/cities"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0-beta2"]
                 [org.clojure/test.check "0.10.0-alpha2"]]
  :profiles {:dev {:source-paths ["dev"]
                   :target-path "target/dev"
                   :dependencies [[org.clojure/tools.namespace "0.2.11"]]}}
  :repl-options {:init-ns user})
