(ns misplaced-villages.state-test
  (:require
   [clojure.test :refer :all]
   [misplaced-villages.state :as game :refer [move
                                              wager-card
                                              number-card
                                              empty-piles]]))

(deftest finding-possible-moves
  (is
   (= [(move "mike" (wager-card :blue) :discard-pile :blue)
       (move "mike" (wager-card :blue) :discard-pile :draw-pile)
       (move "mike" (wager-card :blue) :expedition :draw-pile)]
      (game/possible-moves
       {:game/turn "mike"
        :game/moves []
        :game/discard-piles empty-piles
        :game/draw-pile []
        :game/players {"mike" {:player/hand [(wager-card :blue)]
                               :player/expeditions empty-piles}}}))
   "If we have a wager card and an expedition has no number card, the wager card can be played.")
  (is
   (= [(move "mike" (wager-card :blue) :discard-pile :blue)
       (move "mike" (wager-card :blue) :discard-pile :draw-pile)]
      (game/possible-moves
       {:game/turn "mike"
        :game/moves []
        :game/discard-piles empty-piles
        :game/draw-pile []
        :game/players {"mike" {:player/hand [(wager-card :blue)]
                              :player/expeditions {:green []
                                                   :red []
                                                   :blue [(number-card :blue 2)]
                                                   :white []
                                                   :yellow []}}}}))
   "If there is a number card in an expedition, no wager card can be played."))


(deftest determining-if-game-is-over
  (is (game/game-over? {:game/turn "mike"
                   :game/moves []
                   :game/discard-piles empty-piles
                   :game/draw-pile []
                   :game/players {"mike" {:player/hand [(wager-card :blue)]
                                          :player/expeditions empty-piles}}})
      "If there are no cards in the draw pile, the game is over.")
  (is (not (game/game-over?  {:game/turn "mike"
                         :game/moves []
                         :game/discard-piles empty-piles
                         :game/draw-pile [(wager-card :blue)]
                         :game/players {"mike" {:player/hand []
                                                :player/expeditions empty-piles}}}))
      "If there are cards in the draw pile, the game isn't over."))

(deftest scoring-expeditions
  (is
   (= 0 (game/expedition-score []))
   "If no cards are placed in an expedition, no costs are incurred.")

  (is
   (= -20 (game/expedition-score [(wager-card :blue)]))
   "Starting an expedition costs 20 points.")

  (is
   (= 0 (game/expedition-score [(number-card :blue 2)
                                 (number-card :blue 8)
                                 (number-card :blue 10)]))
   "Number add points to the expedition.")

  (is
   (= 0 (game/expedition-score [(wager-card :blue)
                                (number-card :blue 10)]))
   "One wager card multiples the score by 2.")

  (is
   (= 10 (game/expedition-score [(wager-card :blue)
                                 (wager-card :blue)
                                 (number-card :blue 10)]))
   "Two wager cards multiply the score by 3.")

  (is
   (= 20 (game/expedition-score [(wager-card :blue)
                                 (wager-card :blue)
                                 (wager-card :blue)
                                 (number-card :blue 10)]))
   "Three wager cards multiply the score by 4.")

  (is (= 80 (game/expedition-score [(wager-card :blue)
                                    (wager-card :blue)
                                    (wager-card :blue)
                                    (number-card :blue 2)
                                    (number-card :blue 3)
                                    (number-card :blue 4)
                                    (number-card :blue 5)
                                    (number-card :blue 6)]))
      "If an expedition contains 8 or more cards, a bonus of 20 is awarded."))
