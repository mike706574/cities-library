(ns milo.full_game-test
  (:require
   [clojure.edn :as edn]
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]
   [clojure.test.check :as tc]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [milo.card :as card]
   [milo.player :as player]
   [milo.move :as move]
   [milo.game :as game]
   [clojure.walk :as walk]
   [clojure.test :as test]
   [milo.data :as data]))

(stest/instrument)

(comment
  "Generating decks"
  (vec (map card/short-card (take 20 (shuffle card/deck)))))

(def deck-1
  [(card/number :yellow 7)
   (card/number :yellow 6)
   (card/number :yellow 10)
   (card/number :yellow 8)
   (card/wager :yellow 3)
   (card/number :green 3)
   (card/wager :blue 3)
   (card/number :green 10)
   (card/number :blue 3)
   (card/number :yellow 2)
   (card/number :blue 9)
   (card/number :white 9)
   (card/number :white 5)
   (card/number :yellow 9)
   (card/wager :white 2)
   (card/wager :white 1)
   (card/number :yellow 4)
   (card/number :blue 8)
   (card/wager :red 2)
   (card/wager :red 3)])

(def deck-2
  [(card/number :red 4)
   (card/wager :yellow 1)
   (card/number :white 4)
   (card/wager :green 3)
   (card/wager :blue 1)
   (card/number :yellow 9)
   (card/number :red 7)
   (card/number :green 6)
   (card/number :white 2)
   (card/wager :white 2)
   (card/number :white 7)
   (card/number :red 10)
   (card/number :green 2)
   (card/wager :yellow 2)
   (card/number :yellow 8)
   (card/number :blue 7)
   (card/number :red 5)
   (card/number :yellow 5)
   (card/number :white 10)
   (card/number :white 9)])

(def deck-3
  [(card/number :blue 7)
   (card/wager :red 2)
   (card/number :green 10)
   (card/number :white 3)
   (card/number :red 4)
   (card/number :white 2)
   (card/number :green 6)
   (card/wager :white 1)
   (card/number :white 5)
   (card/wager :blue 1)
   (card/number :red 9)
   (card/number :yellow 3)
   (card/wager :yellow 2)
   (card/number :green 9)
   (card/number :white 6)
   (card/number :blue 9)
   (card/wager :blue 2)
   (card/number :red 10)
   (card/number :white 7)
   (card/number :red 3)])

(def foo {::game/cards {:milo.player/expeditions {:blue [],
                                                           :green [],
                                                           :red [],
                                                           :white [],
                                                           :yellow []},
                   :milo.player/hand '({:milo.card/color :white,
                                                     :milo.card/number 2,
                                                     :milo.card/type :number}
                                                    {:milo.card/color :white,
                                                     :milo.card/number 2,
                                                     :milo.card/type :wager}
                                                    {:milo.card/color :white,
                                                     :milo.card/number 7,
                                                     :milo.card/type :number}
                                                    {:milo.card/color :red,
                                                     :milo.card/number 10,
                                                     :milo.card/type :number}
                                                    {:milo.card/color :green,
                                                     :milo.card/number 2,
                                                     :milo.card/type :number}
                                                    {:milo.card/color :yellow,
                                                     :milo.card/number 2,
                                                     :milo.card/type :wager}
                                                    {:milo.card/color :yellow,
                                                     :milo.card/number 8,
                                                     :milo.card/type :number}
                                                    {:milo.card/color :blue,
                                                     :milo.card/number 7,
                                                     :milo.card/type :number})}}


  )

(defn card?
  [x]
  (and (map? x)
       (= (set (keys x)) #{::card/type ::card/color ::card/number})))

(walk/postwalk
 (fn [x]
   (cond
     (card? x) (card/literal-card x)
     (list? x) (vec x)
     (= x ::player/hand) ::player/hand
     :else x))
 foo)





(test foo-game (game/game ["Mike" "Abby"] [deck-1 deck-2 deck-3] 4))

