(ns milo.move-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.spec.gen :as gen]))

(stest/instrument)
(stest/check (stest/enumerate-namespace 'milo.move))
