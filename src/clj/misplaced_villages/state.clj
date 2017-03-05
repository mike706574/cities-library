(ns misplaced-villages.state
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]
            [clojure.math.combinatorics :as combo]
            [dire.core :refer [with-pre-hook!]]
            [taoensso.timbre :as log]
            [clojure.string :as str]))

(def colors #{:green :red :blue :white :yellow})
(def color? colors)
(def type? #{:wager :number})
(def number? #{2 3 4 5 6 7 8 9 10})

(s/def :card/type type?)
(s/def :card/color color?)
(s/def :card/number number?)

(s/def :game/wager-card (s/keys :req [:card/type :card/color]))
(s/def :game/number-card (s/keys :req [:card/type :card/color :card/number]))

(defmulti card-type :card/type)
(defmethod card-type :wager [_] (s/keys :req [:card/type :card/color]))
(defmethod card-type :number [_] (s/keys :req [:card/type :card/color :card/number]))

(s/def :game/card (s/multi-spec card-type :card/type))

(defn number-card
  [color number]
  {:card/type :number :card/color color :card/number number})

(s/fdef number-card
  :args (s/cat :color :card/color :number :card/number)
  :ret :game/number-card)

(defn wager-card
  [color]
  {:card/type :wager :card/color color})

(s/fdef wager-card
  :args (s/cat :color :card/color)
  :ret :game/card)

(defn cards-for-color
  [color]
  (concat
   (take 3 (repeat (wager-card color)))
   (map (partial number-card color) (range 2 11))))

(def card-set (reduce concat (map cards-for-color colors)))
(def empty-piles (into {} (map #(vector % []) colors)))

(s/def :player/id string?)

(s/def :player/expedition (s/and (s/coll-of :game/card)
                                 (fn [expedition]
                                   (let [distinct-colors (set (map :card/color expedition))]
                                     (<= 0 (count distinct-colors) 1)))))

(s/def :player/expeditions (s/map-of :card/color (s/spec :player/expedition)))
(s/def :player/hand (s/coll-of :game/card))
(s/def :player/data (s/keys :req [:player/hand
                                  :player/expeditions]))

(s/def :game/players (s/map-of :player/id :player/data))

(s/def :game/draw-pile (s/coll-of :game/card))
(s/def :game/discard-pile (s/coll-of :game/card))
(s/def :game/discard-piles (s/map-of :card/color :game/discard-pile))

(s/def :game/turn :player/id)

(s/def :game/deck (s/coll-of :game/card))

;; A move consists of two phases: placing and drawing

(def destinations #{:expedition :discard-pile})
(def sources (conj colors :draw-pile))
(s/def :move/source sources)
(s/def :move/destination destinations)
(s/def :game/move (s/keys :req [:player/id
                                :game/card
                                :move/destination
                                :move/source]))
(defn move
  [player-id card destination source]
  {:player/id player-id
   :game/card card
   :move/destination destination
   :move/source source})

(s/fdef move
  :args (s/cat :player-id :player/id
               :card :game/card
               :destination :move/destination
               :source :move/source)
  :ret :game/move)

(s/def :game/moves (s/* :game/move))

;; Status after an attempted move
(s/def :game/status #{:wrong-player
                      :card-not-in-hand
                      :not-in-hand
                      :expedition-underway
                      :discard-empty
                      :too-low
                      :taken})

;; Options
(s/def :option/deck-size (s/and integer? #(<= 0 % (count card-set))))
(s/def :option/hand-size (s/and integer? pos?))
(s/def :game/options (s/keys :req [:option/deck-size
                                   :option/hand-size]))

;; Full game state
(s/def :game/state
  (s/and (s/keys :req [:game/turn
                       :game/players
                       :game/discard-piles
                       :game/moves
                       :game/draw-pile])))

(defn str-card
  "Builds a string representation of a card useful for human consumption."
  [{:keys [:card/type :card/color :card/number]}]
  (str (name color) "-" (or number "wager")))

(defn str-hand
  [hand]
  (doall (map str-card hand)))

(defn str-state
  "Builds a string representation of the game state for human consumption.."
  [state]
  (let [{:keys [:game/deck :game/turn
                :game/moves :game/players]} state]
    {:deck (str "There are " (count deck) " cards left in the deck.")
     :turn (str "It is " (name turn) "'s turn.")
     :moves (str "There have been " (count moves) " moves so far.")
     :hands (into {} (map (fn [[player data]] [player (str/join ", " (str-hand (:player/hand data)))]) players))}))

(defn start-game
  "Create initial game state."
  [player-ids]
  (let [hand-size 8
        deck-size 44
        full-deck (take deck-size (shuffle card-set))
        player-count (count player-ids)
        deal-count (* hand-size player-count)
        hands (partition hand-size (take deal-count full-deck))
        draw-pile (drop deal-count full-deck)
        player-data (map (fn [hand] {:player/hand hand
                                     :player/expeditions empty-piles}) hands)
        players (zipmap player-ids player-data)]
      {:game/turn (rand-nth player-ids)
       :game/players players
       :game/discard-piles empty-piles
       :game/draw-pile draw-pile
       :game/moves []}))

(s/fdef start-game
  :args (s/cat :player-ids (s/coll-of :player/id))
  :ret :game/state)

(defn right-player?
  "Checks if the move is being made by the player whose turn it is."
  [state move]
  (= (:game/turn state) (:player/id move)))

(s/fdef start-game
        :args (s/cat :player-ids (s/coll-of :player/id))
        :ret :game/state)

(defn validate-placement
  "Validates card placement. Returns nil if valid, keyword error condition if invalid."
  [state move]
  (let [{:keys [:player/id :game/card :move/destination]} move
        player (get-in state [:game/players id])
        {:keys [:player/hand :player/expeditions]} player]
    ;; You can't play a card you don't have.
    (if-not (some #(= % card) hand)
      :not-in-hand
      ;; You can discard any card, no matter what.
      (when (= destination :expedition)
        (let [{card-type :card/type
               card-color :card/color
               card-number :card/number} card
              expedition (get expeditions card-color)]
          ;; You can play any card if an expedition hasn't been started yet.
          (when-let [top-card (last expedition)]
            (let [{top-card-type :card/type
                   top-card-number :card/number} top-card]
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
  (let [{:keys [:move/source
                :move/destination
                :game/card]} move]
    (when-not (or ;; It's always OK to draw from the draw pile.
                  (= source :draw-pile)
                  ;; We're OK if we just discarded a card with the same color as
                  ;; the discard pile we're drawing from.
                  (and (= destination :discard-pile)
                       (= (:card/color card) source))
                  ;; We're OK If the discard pile we're drawing from has cards
                  ;; in it.
                  (seq (get-in state [:game/discard-piles state])))
      :discard-pile-not-empty)))

(defn validate-move
  [state move]
  (if-let [placement-issue (validate-placement state move)]
    placement-issue
    (validate-draw state move)))

(defn card-in-hand?
  [state move]
  (let [{:keys [:player/id :game/card]} move
        hand (get-in state [:game/players id :player/hand])]
    (log/info "Player: " id)
    (log/info "Hand:" (str-hand hand))
    (log/info "Card:" card)
    (some #(= % card) hand)))

(defn remove-first
  "Removes the first instance of item from coll."
  [coll item]
  (let [[before from] (split-with (partial not= item) coll)]
    (concat before (rest from))))

(defn draw-card
  "Draws a card, either from the draw pile or the discard pile specified in move."
  [state move]
  (let [source (:move/source move)]
    (if (= source :deck)
      (let [[top-card & rest-of-deck] (:game/deck state)]
        [top-card (assoc state :game/deck state)])
      (let [discard (get-in state [:game/discards source])
            top-card (last discard)
            discard (drop (dec (count discard)) discard)]
        [top-card (assoc-in state [:game/discards source] discard)]))))

(defn remove-first-from-hand
  "Removes the first instance of card from the given player's hand."
  [state player-id card]
  (update-in state [:game/players player-id :player/hand] #(remove-first % card)))

(defn add-to-hand
  "Adds an instance of card to the given player's hand."
  [state player-id card]
  (update-in state [:game/players player-id :player/hand] #(conj % card)))

(defn discard-card
  "Places an instance of card into the appropriate discard pile based on card color."
  [state card]
  (update-in state [:game/discards (:card/color card)] #(conj % card)))

(defn play-card
  "Places an instance of card into the player's appropriate expedition based on card color."
  [state player card]
  (update-in state [:game/expeditions player (:card/color card)] #(conj % card)))

(defn make-move
  "Performs the given move. Moves consist of two phases: placement and draw."
  [state move]
  (println state)
  (let [player-id (:player/id move)
        card (:game/card move)
        place-card (case (:move/destination move)
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
    {:control/status :wrong-player}
    (if-not (card-in-hand? state move)
      {:control/status :card-not-in-hand}
      (if-let [move-issue (validate-move state move)]
        {:control/status move-issue}
        {:control/status :taken
         :game/state (make-move state move)}))))

(s/fdef take-turn
  :args (s/cat :state :game/state :move :game/move)
  :ret (s/keys :req [:control/status
                     :game/state]))

(defn potential-moves
  [state]
  (let [turn (:game/turn state)
        _ (println turn)
        hand (get-in state [:game/players turn :player/hand])
        _ (println hand)
        combos (combo/cartesian-product hand destinations sources)]
    (map #(apply move turn %) combos)))

(defn possible-moves
  [state]
  (let [potential-moves (potential-moves state)]
    (log/info (str "Generated " (count potential-moves) " potential moves."))
    (filter
     (fn [potential-move]
       (let [issue (validate-move state potential-move)]
         (nil? issue)))
     potential-moves)))

(s/fdef possible-moves
  :args (s/cat :state :game/state)
  :ret :game/moves)

(defn game-over?
  [state]
  (empty? (:game/draw-pile state)))

(def print-separator
  #(doseq [x (take 3 (repeat (str/join (take 80 (repeat "-")))))] (println x)))

(defn expedition-score
  [expedition]
  (if (empty? expedition)
    0
    (let [wage-count (count (filter #(= (:card/type %) :wager) expedition))
          wage-factor (inc wage-count)
          numbers (map :card/number (filter #(= (:card/type %) :number) expedition))
          sum (reduce + numbers)
          bonus (if (> (count expedition) 7) 20 0)]
      (log/info "Number of wager cards:" wage-count)
      (log/info (* wage-factor sum))
      (+ (* wage-factor sum)
         -20
         bonus))))

(s/fdef expedition-score
  :args (s/cat :expedition :player/expedition)
  :ret integer?)

(stest/instrument)
