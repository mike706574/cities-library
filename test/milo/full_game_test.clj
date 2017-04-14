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

(defn cardify
  [x]
  (walk/postwalk #(if (card? %) (card/literal-card %) %) x))

(def game (game/game ["mike" "abby"] [deck-1 deck-2 deck-3] 4))

(def first-8
  [(card/number :yellow 7)
   (card/number :yellow 6)
   (card/number :yellow 10)
   (card/number :yellow 8)
   (card/wager :yellow 3)
   (card/number :green 3)
   (card/wager :blue 3)
   (card/number :green 10)])

(def second-8
  [(card/number :blue 3)
   (card/number :yellow 2)
   (card/number :blue 9)
   (card/number :white 9)
   (card/number :white 5)
   (card/number :yellow 9)
   (card/wager :white 2)
   (card/wager :white 1)])

(def last-4
  [(card/number :yellow 4)
   (card/number :blue 8)
   (card/wager :red 2)
   (card/wager :red 3)])

(def deck-1 (concat first-8 second-8 last-4))



(game/round-cards
 (::game/round game))

(defmacro breakdown
  [game & body]
  `(let [~'players (::game/players game)
         ~'round (::game/round game)
         ~'past-rounds (::game/past-rounds game)
         ~'remaining-rounds (::game/remaining-rounds game)

         ;; Last Round
         ~'last-round (last ~'past-rounds)
         ~'last-round-cards (when ~'last-round (game/round-cards ~'last-round))
         ~'last-discard-piles (::game/discard-piles ~'last-round)
         ~'last-moves (::game/moves ~'last-round)
         ~'last-draw-pile (::game/draw-pile ~'last-round)
         ~'last-player-data (::game/player-data ~'last-round)

         ;; Current Round
         ~'turn (::game/turn ~'round)
         ~'round-cards (game/round-cards ~'round)
         ~'discard-piles (::game/discard-piles ~'round)
         ~'moves (::game/moves ~'round)
         ~'draw-pile (::game/draw-pile ~'round)
         ~'player-data (::game/player-data ~'round)

         ;; Mike
         ~'mike-data (get ~'player-data "mike")
         ~'mike-hand (::player/hand ~'mike-data)
         ~'mike-expeditions (::player/expeditions ~'mike-data)

         ;; Abby
         ~'abby-data (get ~'player-data "abby")
         ~'abby-hand (::player/hand ~'abby-data)
         ~'abby-expeditions (::player/expeditions ~'abby-data)]
     ~@body))

(deftest something
  (testing "something"
    (breakdown game
               (is (= ["mike" "abby"] players))
               (is (= "mike" turn))
               (is (empty? past-rounds))
               (is (= 2 (count remaining-rounds)))
               (is (= (set deck-1) round-cards))
               (is (= first-8 mike-hand))
               (is (= second-8 abby-hand))
               (is (= last-4 draw-pile))
               (is {:white [] :yellow [] :green [] :red [] :blue []} discard-piles)
               (is {:white [] :yellow [] :green [] :red [] :blue []}
                   (empty? mike-expeditions))
               (is {:white [] :yellow [] :green [] :red [] :blue []}
                   (empty? abby-expeditions)))

    (is (= :wrong-player (-> game
                             (game/take-turn (move/exp* "abby" (card/wager :yellow 3)))
                             ::game/status))
        "You can't take a turn when it's not your turn.")

    (is (= :card-not-in-hand (-> game
                                 (game/take-turn (move/exp* "mike" (card/number :green 6)))
                                 ::game/status))
        "You can't play a card that isn't in your hand.")

    (is (= :card-not-in-hand (-> game
                                 (game/take-turn (move/disc* "mike" (card/number :green 6)))
                                 ::game/status))
        "You can't discard a card that isn't in your hand.")

    (is (= :discard-pile-empty (-> game
                                   (game/take-turn (move/disc "mike" (card/wager :yellow 3) :green))
                                   ::game/status))
        "You can't take from an empty discard pile.")













    )
  )
