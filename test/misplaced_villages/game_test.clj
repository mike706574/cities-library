(ns misplaced-villages.game-test
  (:require
   [clojure.test :refer :all]
   [misplaced-villages.card :as card]
   [misplaced-villages.player :as player]
   [misplaced-villages.move :as move]
   [misplaced-villages.game :as game]))

(deftest finding-possible-moves
  (is
   (= [(move/move "mike" (card/wager :blue) :discard-pile :blue)
       (move/move "mike" (card/wager :blue) :discard-pile :draw-pile)
       (move/move "mike" (card/wager :blue) :expedition :draw-pile)]
      (game/possible-moves
       {::game/turn "mike"
        ::game/moves []
        ::game/discard-piles card/empty-piles
        ::game/draw-pile []
        ::game/player-data {"mike" {::player/hand [(card/wager :blue)]
                                    ::player/expeditions card/empty-piles}}}))
   "If we have a wager card and an expedition has no number card, the wager card can be played.")
  (is
   (= [(move/move "mike" (card/wager :blue) :discard-pile :blue)
       (move/move "mike" (card/wager :blue) :discard-pile :draw-pile)]
      (game/possible-moves
       {::game/turn "mike"
        ::game/moves []
        ::game/discard-piles card/empty-piles
        ::game/draw-pile []
        ::game/player-data {"mike" {::player/hand [(card/wager :blue)]
                                    ::player/expeditions {:green []
                                                          :red []
                                                          :blue [(card/number :blue 2)]
                                                          :white []
                                                          :yellow []}}}}))
   "If there is a number card in an expedition, no wager card can be played."))


(deftest determining-if-game-is-over
  (is (game/round-over? {::game/turn "mike"
                         ::game/moves []
                         ::game/discard-piles card/empty-piles
                         ::game/draw-pile []
                         ::game/players {"mike" {::player/hand [(card/wager :blue)]
                                                 ::player/expeditions card/empty-piles}}})
      "If there are no cards in the draw pile, the round is over.")
  (is (not (game/round-over? {::game/turn "mike"
                              ::game/moves []
                              ::game/discard-piles card/empty-piles
                              ::game/draw-pile [(card/wager :blue)]
                              ::game/players {"mike" {::player/hand []
                                                      ::player/expeditions card/empty-piles}}}))
      "If there are cards in the draw pile, the round isn't over."))


(deftest scoring-expeditions
  (is
   (= 0 (game/expedition-score []))
   "If no cards are placed in an expedition, no costs are incurred.")

  (is
   (= -20 (game/expedition-score [(card/wager :blue)]))
   "Starting an expedition costs 20 points.")

  (is
   (= 0 (game/expedition-score [(card/number :blue 2)
                                (card/number :blue 8)
                                (card/number :blue 10)]))
   "Number add points to the expedition.")

  (is
   (= 0 (game/expedition-score [(card/wager :blue)
                                (card/number :blue 10)]))
   "One wager card multiples the score by 2.")

  (is
   (= 10 (game/expedition-score [(card/wager :blue)
                                 (card/wager :blue)
                                 (card/number :blue 10)]))
   "Two wager cards multiply the score by 3.")

  (is
   (= 20 (game/expedition-score [(card/wager :blue)
                                 (card/wager :blue)
                                 (card/wager :blue)
                                 (card/number :blue 10)]))
   "Three wager cards multiply the score by 4.")

  (is (= 80 (game/expedition-score [(card/wager :blue)
                                    (card/wager :blue)
                                    (card/wager :blue)
                                    (card/number :blue 2)
                                    (card/number :blue 3)
                                    (card/number :blue 4)
                                    (card/number :blue 5)
                                    (card/number :blue 6)]))
      "If an expedition contains 8 or more cards, a bonus of 20 is awarded."))
