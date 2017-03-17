(ns misplaced-villages.report
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [clojure.string :as str]
   [misplaced-villages.card :as card]
   [misplaced-villages.player :as player]
   [misplaced-villages.move :as move]
   [misplaced-villages.game :as game]))

(defn str-card
  "Builds a string representation of a card."
  [{:keys [::card/type ::card/color ::card/number]}]
  (str (name color) "-" (or number "wager")))

(defn str-hand
  [hand]
  (doall (map str-card hand)))

(defn str-round
  "Builds a string representation of a round."
  [state]
  (let [{:keys [::draw-pile
                ::turn
                ::moves
                ::player-data]} state
        possible-moves (game/possible-moves state)]
    (str/join "\n" [(case (count draw-pile)
                      0 "There are no cards left in the deck."
                      1 "There is 1 card left in the deck."
                      (str "There are " (count draw-pile) " cards left in the deck."))
                    (str "It is " (name turn) "'s turn.")
                    (case (count moves)
                      0 (str "There have been no moves so far.")
                      1 (str "There has been 1 move so far.")
                      (str "There have been " (count moves) " moves so far."))
                    ""
                    (str/join "\n" (map
                                    (fn [[player data]]
                                      (str player ": " (str/join ", " (str-hand (::player/hand data)))))
                                    player-data))
                    ""
                    "Possible moves:"
                    (str/join "\n" (map
                                    (fn [move]
                                      (let [{:keys [::player/id
                                                    ::move/destination
                                                    ::move/source
                                                    ::card/card]} move]
                                        (str (case destination
                                               :expedition "Play"
                                               :discard-pile "Discard")
                                             " "
                                             (str-card card)
                                             ", "
                                             (if (= source :draw-pile)
                                               "draw new card"
                                               (str "Take " (name source) " discard")))))
                                    possible-moves))

                    ])))

(defn str-game
  "Builds a string representation of the game state."
  [state]
  (let [{rounds ::game/rounds} state
        round-number (count rounds)
        round (last rounds)]
    (str/join "\n" [(str "Round #" round-number)
                    ""
                    (str-round round)])))
