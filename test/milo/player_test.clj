(ns milo.player-test
  (:require
   [clojure.spec.test :as stest]
   [clojure.spec.gen :as gen]
   [milo.player :as player]))

(stest/instrument)

(stest/check 'milo.player/uniform-color?)
(stest/check 'milo.player/wagers-before-numbers?)
(stest/check 'milo.player/under-wager-limit?)
(stest/check 'milo.player/numbers-in-order?)

(->> (stest/enumerate-namespace 'milo.player)
     (stest/check))
