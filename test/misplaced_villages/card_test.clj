(ns misplaced-villages.card-test
  (:require
   [clojure.test :refer :all]
   [clojure.spec.test :as stest]
   [clojure.spec.gen :as gen]
   [misplaced-villages.card :as card]
   [clojure.spec :as s]))

(deftest str-card
  (is (= "blue-5" (card/str-card (card/number :blue 5))))
  (is (= "green-wager-1" (card/str-card (card/wager-1 :green)))))

(stest/instrument)
(stest/check (stest/enumerate-namespace 'misplaced-villages.card))
