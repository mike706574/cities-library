(ns misplaced-villages.game
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]
            [clojure.math.combinatorics :as combo]
            [dire.core :refer [with-pre-hook!]]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [misplaced-villages.card :as card]
            [misplaced-villages.player :as player]
            [misplaced-villages.move :as move]))

(s/def ::player-data (s/map-of ::player/id ::player/data))
(s/def ::discard-piles (s/map-of ::card/color ::card/discard-pile))
(s/def ::turn ::player/id)

(s/def ::draw-pile (s/coll-of ::card/card))
(s/def ::moves (s/* ::move/move))
(s/def ::discard-pile (s/coll-of ::card))

;; Status after an attempted move
(s/def ::status #{:wrong-player
                  :card-not-in-hand
                  :not-in-hand
                  :expedition-underway
                  :discard-empty
                  :too-low
                  :taken})

;; Options
(s/def ::deck-size (s/and integer? #(<= 0 % (count card/deck))))
(s/def ::hand-size (s/and integer? pos?))

;; A round
(s/def ::round
  (s/keys :req [::turn
                ::player-data
                ::discard-piles
                ::moves
                ::draw-pile]))

(s/def ::rounds (s/coll-of ::round))
(s/def ::players (s/coll-of ::player/id))

(s/def ::state
  (s/keys :req [::players
                ::rounds]))

(defn str-card
  "Builds a string representation of a card useful for human consumption."
  [{:keys [::card/type ::card/color ::card/number]}]
  (str (name color) "-" (or number "wager")))

(defn str-hand
  [hand]
  (doall (map str-card hand)))

(defn str-round
  "Builds a string representation of a round for human consumption."
  [state]
  (let [{:keys [::draw-pile
                ::turn
                ::moves
                ::player-data]} state
        possible-moves (possible-moves state)]
    (str/join "\n" [(str "There are " (count draw-pile) " cards left in the deck.")
                    (str "It is " (name turn) "'s turn.")
                    (str "There have been " (count moves) " moves so far.")
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

(defn start-round
  "Create a round."
  [players]
  (let [hand-size 8
        deck-size 60
        full-deck (take deck-size (shuffle card/deck))
        player-count (count players)
        deal-count (* hand-size player-count)
        hands (partition hand-size (take deal-count full-deck))
        draw-pile (drop deal-count full-deck)
        initial-player-data (map (fn [hand] {::player/hand hand
                                             ::player/expeditions card/empty-piles}) hands)
        player-data (zipmap players initial-player-data)]
      {::turn (rand-nth players)
       ::player-data player-data
       ::discard-piles card/empty-piles
       ::draw-pile draw-pile
       ::moves []}))

(s/fdef start-round
  :args (s/cat :players ::players)
  :ret ::round)

(defn right-player?
  "Checks if the move is being made by the player whose turn it is."
  [state move]
  (= (::turn state) (::player/id move)))

(s/fdef start-game
        :args (s/cat :players ::players)
        :ret ::round)

(defn validate-placement
  "Validates card placement. Returns nil if valid, keyword error condition if invalid."
  [state move]
  (let [{:keys [::player/id ::card/card ::move/destination]} move
        player (get-in state [::player-data id])
        {:keys [::player/hand ::player/expeditions]} player]
    ;; You can't play a card you don't have.
    (if-not (some #(= % card) hand)
      :not-in-hand
      ;; You can discard any card, no matter what.
      (when (= destination :expedition)
        (let [{card-type ::card/type
               card-color ::card/color
               card-number ::card/number} card
              expedition (get expeditions card-color)]
          ;; You can play any card if an expedition hasn't been started yet.
          (when-let [top-card (last expedition)]
            (let [{top-card-type ::card/type
                   top-card-number ::card/number} top-card]
              (case top-card-type
                ;; When the top card is a wager, check wage limit.
                :wager (when (and (= card-type :wager)
                                  (= 3 (count expedition)))
                         :wage-limit)
                :number (case card-type
                          ;; You can't play a wager if a number card has been played.
                          :wager :expedition-underway
                          ;; You can only play cards with an equal or greater value than the top card.
                          :number (when (<= card-number top-card-number)
                                    :too-low))))))))))

(defn validate-draw
  "Validates card draw. Returns nil if valid, keyword error condition if invalid."
  [state move]
  (let [{:keys [::move/source
                ::move/destination
                ::card/card]} move]
    (when-not (or ;; It's always OK to draw from the draw pile.
                  (= source :draw-pile)
                  ;; We're OK if we just discarded a card with the same color as
                  ;; the discard pile we're drawing from.
                  (and (= destination :discard-pile)
                       (= (::card/color card) source))
                  ;; We're OK If the discard pile we're drawing from has cards
                  ;; in it.
                  (seq (get-in state [::discard-piles state])))
      :discard-pile-not-empty)))

(defn validate-move
  [state move]
  (if-let [placement-issue (validate-placement state move)]
    placement-issue
    (validate-draw state move)))

(defn card-in-hand?
  [state move]
  (let [{:keys [::player/id ::card/card]} move
        hand (get-in state [::player-data id ::player/hand])]
    (some #(= % card) hand)))

(defn remove-first
  "Removes the first instance of item from coll."
  [coll item]
  (let [[before from] (split-with (partial not= item) coll)]
    (concat before (rest from))))

(defn draw-card
  "Draws a card, either from the draw pile or the discard pile specified in move."
  [state move]
  (let [source (::move/source move)]
    (if (= source :draw-pile)
      (let [[top-card & rest-of-draw-pile] (::draw-pile state)]
        [top-card (assoc state ::draw-pile rest-of-draw-pile)])
      (let [discard (get-in state [::discard-piles source])
            top-card (last discard)
            rest-of-discard-pile (drop (dec (count discard)) discard)]
        [top-card (assoc-in state [::discard-piles source] discard)]))))

(defn remove-first-from-hand
  "Removes the first instance of card from the given player's hand."
  [state player-id card]
  (update-in state [::player-data player-id ::player/hand] #(remove-first % card)))

(defn add-to-hand
  "Adds an instance of card to the given player's hand."
  [state player-id card]
  (update-in state [::player-data player-id ::player/hand] #(conj % card)))

(defn discard-card
  "Places an instance of card into the appropriate discard pile based on card color."
  [state card]
  (update-in state [::discard-piles (:card/color card)] #(conj % card)))

(defn play-card
  "Places an instance of card into the player's appropriate expedition based on card color."
  [state player card]
  (update-in state [::player-data player ::player/expeditions (::card/color card)] #(conj % card)))

(defn make-move
  "Performs the given move. Moves consist of two phases: placement and draw."
  [state move]
  (let [player-id (::player/id move)
        card (::card/card move)
        place-card (case (::move/destination move)
                     :expedition #(play-card % player-id card)
                     :discard #(discard-card % card))
        state (-> state
                  (remove-first-from-hand player-id card)
                  (place-card)
                  (update :moves #(conj % move)))
        [drawn-card state] (draw-card state move)]
    (add-to-hand state player-id drawn-card)))

(defn take-turn
  "Take a turn."
  [state move]
  (if-not (right-player? state move)
    {::status :wrong-player}
    (if-not (card-in-hand? state move)
      {::status :card-not-in-hand}
      (if-let [move-issue (validate-move state move)]
        {::status move-issue}
        {::status :taken
         ::round (make-move state move)}))))

(s/fdef take-turn
  :args (s/cat :state ::round :move ::move/move)
  :ret (s/keys :req [::status
                     ::round]))

(defn potential-moves
  [state]
  (let [turn (::turn state)
        hand (get-in state [::player-data turn ::player/hand])
        combos (combo/cartesian-product hand move/destinations move/sources)]
    (map #(apply move/move turn %) combos)))

(defn possible-moves
  [state]
  (let [potential-moves (potential-moves state)]
    (filter
     (fn [potential-move]
       (let [issue (validate-move state potential-move)]
         (nil? issue)))
     potential-moves)))

(s/fdef possible-moves
  :args (s/cat :state ::round)
  :ret ::moves)

(defn round-over?
  [state]
  (empty? (::draw-pile state)))

(def print-separator
  #(doseq [x (take 3 (repeat (str/join (take 80 (repeat "-")))))] (println x)))

(defn expedition-score
  [expedition]
  (if (empty? expedition)
    0
    (let [wage-count (count (filter #(= (::card/type %) :wager) expedition))
          wage-factor (inc wage-count)
          numbers (map ::card/number (filter #(= (::card/type %) :number) expedition))
          sum (reduce + numbers)
          bonus (if (> (count expedition) 7) 20 0)]
      (+ (* wage-factor sum)
         -20
         bonus))))

(s/fdef expedition-score
  :args (s/cat :expedition ::player/expedition)
  :ret integer?)

(defn start-game
  "Create a game."
  [players]
  {::players players
   ::rounds [(start-round players)]})

(defn str-game
  [state]
  (let [{rounds ::rounds} state
        round-number (count rounds)
        round (last rounds)]
    (str/join "\n" [(str "Round #" round-number)
                    ""
                    (str-round round)])))

(defn take-action
  [state move]
  (let [rounds (::rounds state)
        round-count (count rounds)
        round (last rounds)]
    (if (round-over? (last rounds))
      (if (= round-count 3)
        {::status :game-over ::state state}
        (throw (ex-info "The last round is over, but no new round was created." state)))
      (let [{status ::status round* ::round} (take-turn round move)]
        (if-not (= status :taken)
          {::status status}
          (let [state* (assoc-in state [::rounds round-count] round*)]
            (if-not (round-over? round*)
              {::status :taken
               ::state state*}
              (if (= round-count 3)
                {::status :game-over ::state state}
                (let [new-round (start-round (::players state))
                      state** (update state* ::rounds #(conj % new-round))]
                  {::status :new-round
                   ::state state**})))))))))

(defn log-action
  [state move]
  (let [{:keys [::status ::state] :as response} (take-action state move)]
    (log/info status)
    state))

(comment
  (def game (start-game ["Mike" "Abby"]))
  (str-game game)
  (def game-2 (log-action game (move/move "Mike" (card/wager :green) :expedition :draw-pile)))
  (str-game game-2)
  )

(stest/instrument)
