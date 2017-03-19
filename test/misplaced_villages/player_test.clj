(ns misplaced-villages.player-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.spec.gen :as gen]
   [misplaced-villages.player :as player]))

(stest/instrument)

(stest/check 'misplaced-villages.player/uniform-color?)
(stest/check 'misplaced-villages.player/wagers-before-numbers?)
(stest/check 'misplaced-villages.player/under-wager-limit?)
(stest/check 'misplaced-villages.player/numbers-in-order?)

(->> (stest/enumerate-namespace 'misplaced-villages.player)
     (stest/check))
