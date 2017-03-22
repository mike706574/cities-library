(ns misplaced-villages.game
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [misplaced-villages.card :as card]
   [misplaced-villages.player :as player]
   [misplaced-villages.misc :as misc]
   [misplaced-villages.move :as move]))

(s/def ::player-data (s/map-of ::player/id ::player/data :count 2))
(s/def ::players (s/tuple ::player/id ::player/id))

(s/def ::discard-piles ::card/color-piles)
(s/def ::turn ::player/id)

(s/def ::draw-pile (s/coll-of ::card/card))
(s/def ::moves (s/* ::move/move))

(def statuses #{:game-over
                :round-over
                :invalid-player
                :wrong-player
                :card-not-in-hand
                :expedition-underway
                :discard-empty
                :too-low
                :taken})
(s/def ::status statuses)

;; A round
(defn player-cards
  "Collects all cards from every player's hand and expeditions."
  [{:keys [::player/hand ::player/expeditions]}]
  (into hand (card/combine-piles expeditions)))

(defn all-player-cards
  "Collects all cards from every player's hand and expeditions."
  [player-data]
  (flatten
   (map player-cards (vals player-data))))

(defn round-cards
  "Collects all cards for a round."
  [{:keys [::player-data ::draw-pile ::discard-piles]}]
  (-> (set draw-pile)
      (into (card/combine-piles discard-piles))
      (into (all-player-cards player-data))))

(s/fdef round-cards
    :args (s/cat :round ::round)
    :ret (s/coll-of ::card/card))

(s/def ::round
  (s/and
   (s/keys :req [::turn
                 ::player-data
                 ::discard-piles
                 ::moves
                 ::draw-pile])
   #(= (round-cards %) card/deck)))

(s/def ::past-rounds (s/coll-of ::round :max-count 3))
(s/def ::remaining-rounds (s/coll-of ::round :max-count 2))
(s/def ::players (s/coll-of ::player/id
                            :count 2
                            :distinct true))

(s/def ::state
  (s/keys :req [::players
                ::round
                ::past-rounds
                ::remaining-rounds]))

(defn round
  "Creates a round with the given turn order and deck."
  [players deck]
  (let [hands (partition 8 (take 16 deck))
        draw-pile (drop 16 deck)
        initial-player-data (map (fn [hand] {::player/hand hand
                                             ::player/expeditions card/empty-piles}) hands)
        player-data (zipmap players initial-player-data)]
    {::turn (first players)
     ::players players
     ::player-data player-data
     ::discard-piles card/empty-piles
     ::draw-pile draw-pile
     ::moves []}))

(s/fdef round
  :args (s/cat :players ::players
               :deck ::card/deck)
  :ret ::round)

(defn rand-round
  "Creates a round with a random turn order and a shuffled deck."
  [players]
  (round (shuffle players) (shuffle card/deck)))

(s/fdef rand-round
  :args (s/cat :players ::players)
  :ret ::round)

(defn validate-placement
  "Validates card placement. Returns nil if valid or a keyword describing the error condition if invalid."
  [round move]
  (let [{:keys [::player/id ::card/card ::move/destination]} move
        player (get-in round [::player-data id])
        {:keys [::player/hand ::player/expeditions]} player]
    ;; You can't play a card you don't have.
    (if-not (some #{card} hand)
      :card-not-in-hand
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
  [round move]
  (let [{:keys [::move/source
                ::move/destination
                ::card/card]} move
        ;; It's always OK to draw from the draw pile.
        drawing-from-draw-pile? (= source :draw-pile)
        ;; We're OK if we just discarded a card with the same color as
        ;; the discard pile we're drawing from.
        actively-discarding? (and (= destination :discard-pile)
                                  (= (::card/color card) source))
        discards-available? (not (empty? (get-in round [::discard-piles source])))]
    (when-not (or drawing-from-draw-pile?
                  actively-discarding?
                  discards-available?)
      :discard-pile-empty)))

(defn validate-move
  "Validates a move. Returns nil if valid, keyword error condition if invalid."
  [round move]
  (if-let [placement-issue (validate-placement round move)]
    placement-issue
    (validate-draw round move)))

(defn draw-card
  "Draws a card, either from the draw pile or the discard pile specified in move."
  [round move]
  (let [source (::move/source move)]
    (if (= source :draw-pile)
      (let [[top-card & rest-of-draw-pile] (::draw-pile round)]
        [top-card (assoc round ::draw-pile rest-of-draw-pile)])
      (let [discard-pile (get-in round [::discard-piles source])
            top-card (last discard-pile)
            rest-of-discard-pile (or (butlast discard-pile) [])]
        [top-card (assoc-in round [::discard-piles source] rest-of-discard-pile)]))))

(defn remove-from-hand
  "Removes the first instance of card from the given player's hand."
  [round player-id card]
  (update-in round
             [::player-data player-id ::player/hand]
             (fn [hand] (remove #(= % card) hand))))

(defn add-to-hand
  "Adds an instance of card to the given player's hand."
  [round player-id card]
  (update-in round [::player-data player-id ::player/hand] #(conj % card)))

(defn discard-card
  "Places an instance of card into the appropriate discard pile based on card color."
  [round card]
  (update-in round [::discard-piles (::card/color card)] #(conj % card)))

(defn play-card
  "Places an instance of card into the player's appropriate expedition based on card color."
  [round player card]
  (update-in round [::player-data player ::player/expeditions (::card/color card)] #(conj % card)))

(defn make-move
  "Performs the given move. Moves consist of two phases: placement and draw."
  [round move]
  (let [player-id (::player/id move)
        card (::card/card move)
        place-card (case (::move/destination move)
                     :expedition #(play-card % player-id card)
                     :discard-pile #(discard-card % card))
        round (-> round
                  (remove-from-hand player-id card)
                  (place-card)
                  (update ::moves #(conj % move)))
        [drawn-card round] (draw-card round move)
        round-after (add-to-hand round player-id drawn-card)]
    round-after))

(s/fdef make-move
  :args (s/cat :round ::round :move ::move/move)
  :ret ::round)

(defn swap-turn
  "Makes it the other player's turn."
  [round players]
  (update round ::turn #(misc/cycle-after players %)))

(s/fdef swap-turn
  :args (s/cat :round ::round :players ::players)
  :ret ::round)

(defn round-over?
  "Returns true if the round is over."
  [round]
  (empty? (::draw-pile round)))

(defn game
  "Creates a game with given turn order and decks."
  [players [deck-1 deck-2 deck-3]]
  {::players players
   ::round (round players deck-1)
   ::past-rounds []
   ::remaining-rounds [(round (reverse players) deck-2)
                       (round players deck-3)]})

(s/fdef game
  :args (s/cat :players ::players
               :decks (s/coll-of ::card/deck :count 3))
  :ret ::state)

(defn rand-game
  "Create a game with a random turn order and shuffled decks."
  [players]
  (game
   (shuffle players)
   (repeatedly 3 #(shuffle card/deck))))

(s/fdef rand-game
  :args (s/cat :players ::players)
  :ret ::state)

(defn game-over?
  "Returns true if the game is over."
  [{round ::round remaining-rounds ::remaining-rounds}]
  (and (nil? round) (empty? remaining-rounds)))

(defn valid-player?
  "Returns true if the move is being made by a valid player, otherwise false."
  [state move]
  (boolean (some #{(::player/id move)} (::players state))))

(defn right-player?
  "Returns true if the move is being made by the player whose turn it is."
  [state move]
  (let [turn (-> state ::round ::turn)
        player (::player/id move)]
    (= turn player)))

(defn take-turn
  "Takes a turn."
  [state move]
  (cond
    (game-over? state) {::status :game-over ::state state}
    (not (valid-player? state move)) {::status :invalid-player ::state state}
    (not (right-player? state move)) {::status :wrong-player ::state state}
    :else
    (if-let [move-issue (validate-move (::round state) move)]
      {::status move-issue ::state state}
      (let [round* (-> state
                       ::round
                       (make-move move)
                       (swap-turn (::players state)))]
        (if (round-over? round*)
          (let [[new-round & remaining-rounds] (::remaining-rounds state)
                past-rounds (conj (::past-rounds state) round*)
                state* (assoc state
                              ::round new-round
                              ::past-rounds past-rounds
                              ::remaining-rounds remaining-rounds)]
            {::status (if new-round :round-over :game-over)
             ::state state*})
          {::status :taken
           ::state (assoc state ::round round*)})))))

(s/def ::response (s/keys :req [::status ::state]))

(s/fdef take-turn
  :args (s/cat :state ::state :move ::move/move)
  :ret ::response)

(s/def ::draw-count integer?)
(s/def ::draw-count integer?)
(s/def ::opponent ::player/id)
(s/def ::opponent-expeditions ::player/expeditions)
(s/def ::cards-remaining integer?)
(s/def ::available-discards (s/map-of ::card/color ::card/card))

(s/def ::player-state (s/keys :req [::turn
                                    ::player/id
                                    ::player/hand
                                    ::player/expeditions
                                    ::opponent
                                    ::opponent-expeditions
                                    ::moves
                                    ::available-discards
                                    ::past-rounds
                                    ::cards-remaining]))

(defn for-player
  [{:keys [::players ::round ::past-rounds ::remaining-rounds]} player]
  (let [{:keys [::turn ::moves ::discard-piles ::player-data ::draw-pile]} round
        {:keys [::player/hand ::player/expeditions]} (get player-data player)
        opponent (first (filter #(not= % player) players))
        opponent-expeditions (get-in player-data [opponent ::player/expeditions])
        cards-remaining (count draw-pile)
        available-discards (into [] (comp (map val)
                                          (map last)
                                          (filter identity))
                                 discard-piles)]
    {::turn turn
     ::player/id player
     ::player/hand hand
     ::player/expeditions expeditions
     ::opponent opponent
     ::opponent-expeditions opponent-expeditions
     ::moves moves
     ::available-discards available-discards
     ::past-rounds past-rounds
     ::cards-remaining cards-remaining}))

(s/fdef for-player
  :args (s/cat :state ::state :player ::player/id)
  :ret ::player-state)
