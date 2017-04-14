(ns milo.game-test
  (:require
   [clojure.test :refer :all]
   [milo.card :as card]
   [milo.player :as player]
   [milo.move :as move]
   [milo.data :as data]))

(deftest possible-moves-1
  (is
   (= [(move/move "Mike" (card/wager-1 :blue) :discard-pile :blue)
       (move/move "Mike" (card/wager-1 :blue) :discard-pile :draw-pile)
       (move/move "Mike" (card/wager-1 :blue) :expedition :draw-pile)]
      (data/possible-moves
       {::game/turn "Mike"
        ::game/moves []
        ::game/discard-piles card/empty-piles
        ::game/draw-pile []
        ::game/players ["Mike" "Abby"]
        ::game/player-data {"Mike" {::player/hand [(card/wager-1 :blue)]
                                    ::player/expeditions card/empty-piles}}})))
  "If we have a wager card and an expedition has no number card, the wager card can be played.")

(deftest possible-moves-2
  (is
   (= [(move/move "Mike" (card/wager-1 :blue) :discard-pile :blue)
       (move/move "Mike" (card/wager-1 :blue) :discard-pile :draw-pile)]
      (data/possible-moves
       {::game/turn "Mike"
        ::game/moves []
        ::game/discard-piles card/empty-piles
        ::game/draw-pile []
        ::game/player-data {"Mike" {::player/hand [(card/wager-1 :blue)]
                                    ::player/expeditions {:green []
                                                          :red []
                                                          :blue [(card/number :blue 2)]
                                                          :white []
                                                          :yellow []}}}}))
   "If there is a number card in an expedition, no wager card can be played."))
