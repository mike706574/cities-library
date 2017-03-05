(ns misplaced-villages.state
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]
            [clojure.core.match :refer [match]]
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

(stest/instrument `number-card)

(defn wager-card
  [color]
  {:card/type :wager :card/color color})

(s/fdef wager-card
  :args (s/cat :color :card/color)
  :ret :game/card)

(stest/instrument `wager-card)

(defn cards-for-color
  [color]
  (concat
   (take 3 (repeat (wager-card color)))
   (map (partial number-card color) (range 2 11))))

(def card-set (reduce concat (map cards-for-color colors)))
(def empty-stacks (into {} (map #(vector % []) colors)))

(s/def :player/name (s/and string? (complement str/blank?)))
(s/def :player/expedition (s/coll-of :game/card))
(s/def :player/expeditions (s/map-of :card/color (s/spec :player/expedition)))
(s/def :player/hand (s/coll-of :game/card))
(s/def :player/data (s/keys :req [:player/hand
                                  :player/expeditions]))

(s/def :game/players (s/map-of :player/name :player/data))

(s/def :game/draw-pile (s/coll-of :game/card))
(s/def :game/discard-piles (s/map-of :card/color :card/discard))

(s/def :game/turn :game/player)

(s/def :game/deck (s/coll-of :game/card))

;; A move consists of two phases: placing and drawing
(s/def :move/source (conj colors :deck))
(s/def :move/destination #{:expedition :discard-pile})
(s/def :game/move (s/keys :req [:player/namae
                                :game/card
                                :move/destination
                                :move/source]))
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
  (s/keys :req [:game/turn
                :game/players
                :game/discard-piles
                :game/moves
                :game/draw-pipe]))

(defn start-game
  [player-names]
  (let [hand-size 8
        deck-size 44
        full-deck (take deck-size (shuffle card-set))
        player-count (count player-names)
        deal-count (* hand-size player-count)
        hands (partition hand-size (take deal-count full-deck))
        draw-pile (drop deal-count full-deck)
        player-data (map (fn [hand] {:player/hand hand
                                     :player/expeditions empty-stacks}) hands)
        players (zipmap player-names player-data)]
      {:game/turn (rand-nth player-names)
       :game/players players
       :game/discard-piles empty-stacks
       :game/draw-pile draw-pile
       :game/moves []}))

(s/fdef start-game
        :args (s/cat :player-names (s/coll-of :player/name))
        :ret :game/state)

(stest/instrument `start-game)

(defn right-player?
  [state move]
  (= (:game/turn state) (:player/name move)))

(defn validate-placement
  "Validates card placement. Returns nil if valid, keyword error condition if invalid."
  [state move]
  (let [{:keys [:game/player :game/card :move/destination]} move
        player (get-in state [:game/players player])
        {:keys [:player/hand :player/expeditions]} player]
      (if-not (some #(= % card) hand)
        :not-in-hand
        ;; It's always OK to discard.
        (when (= destination :expedition)
          :expedition (let [{:keys [:card/type :card/color :card/number]} card
                            expedition (get expeditions color)
                            top-card (last expedition)]
                        (when-not (or (nil? top-card) (= (:card/type top-card) :wager))
                          (case type
                            :wager :expedition-underway
                            :number (when-not (> number (:card/number top-card))
                                      :too-low))))))))

(defn validate-draw
  "Validates card draw. Returns nil if valid, keyword error condition if invalid."
  [state move]
  (let [source (:move/source move)]
    (when (and (not= (:move/source move) :deck)
               (empty? (get-in state [:game/discard-piles state])))
      :discard-pipe-empty)))

;; TODO: Is this used?
(defn card-in-hand?
  [state move]
  (let [{:keys [:game/player :game/card]} (:game/player move)
        hand (get-in state [:game/players player :player/hand])]
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
            discard (drop discard (dec (count discard)))]
        [top-card (assoc-in state [:game/discards source] discard)]))))

(defn remove-first-from-hand
  "Removes the first instance of card from the given player's hand."
  [state player card]
  (update-in state [:game/hands player] #(remove-first % card)))

(defn add-to-hand
  "Adds an instance of card to the given player's hand."
  [state player card]
  (update-in state [:game/hands player] #(conj % card)))

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
  (let [player (:game/player move)
        card (:game/card move)
        place-card (case (:move/destination move)
                     :expedition #(play-card % player card)
                     :discard #(discard-card % card))
        state (-> state
                  (remove-first-from-hand player card)
                  (place-card)
                  (update :moves #(conj % move)))
        [drawn-card state] (draw-card state move)]
    (add-to-hand state player drawn-card)))

(defn take-turn
  [state move]
  (if-not (right-player? state move)
    {:control/status :wrong-player}
    (if-not (card-in-hand? state move)
      {:control/status :card-not-in-hand}
      (if-let [placement-issue (validate-placement state move)]
        {:control/status placement-issue}
        (if-let [draw-issue (validate-draw state move)]
          {:control/status draw-issue}
          {:control/status :taken
           :game/state (make-move state move)})))))

(s/fdef take-turn
  :args (s/cat :state :game/state :move :game/move)
  :ret (s/keys :req [:control/status
                     :game/state]))

(defn str-card
  [{:keys [:card/type :card/color :card/number]}]
  (str (str/capitalize (name color)) " " (or number "Wager")))

(map str-card (gen/sample (s/gen :game/card) 8))

(defn str-state
  [state]
  (let [{:keys [:game/deck :game/turn
                :game/moves :game/hands]} state
        ]
    {:deck (str "There " (count deck) " cards left in the deck.")
     :turn (str "It is " (name turn) "'s turn.")
     :moves (str "There have been " (count moves) " moves so far.")
     :hands (into {} (map (fn [[player hand]] [player (map str-card hand)]) hands))}))

;;(str-state (start-game [:Mike :Abby]))
