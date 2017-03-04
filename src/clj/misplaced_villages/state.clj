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

(def deck (reduce concat (map cards-for-color colors)))
(def empty-stacks (into {} (map #(vector % []) colors)))

(s/def :game/player keyword?)
(s/def :game/players (s/cat :one :game/player
                            :two :game/player))

(s/def :card/discard (s/coll-of :game/card))
(s/def :game/discards (s/map-of :card/color :card/discard))

(s/def :card/expedition (s/coll-of :game/card))
(s/def :card/expeditions (s/map-of :card/color (s/spec :card/expedition)))
(s/def :game/expeditions (s/map-of :game/player (s/spec :card/expeditions)))

(s/def :game/hand (s/coll-of :game/card :count 8))
(s/def :game/hands (s/map-of :game/player :game/hand))

(s/def :game/turn :game/player)

(s/def :game/deck (s/coll-of :game/card))

(s/def :move/source (conj colors :deck))
(s/def :move/destination #{:expedition :discard})
(s/def :game/move (s/keys :req [:game/player
                                :game/card
                                :move/destination
                                :move/source]))
(s/def :game/status #{:wrong-player
                      :card-not-in-hand
                      :not-in-hand
                      :expedition-underway
                      :discard-empty
                      :too-low
                      :taken})

(s/def :game/moves (s/* :game/move))

(s/def :game/state
  (s/keys :req [:game/turn
                :game/players
                :game/hands
                :game/discards
                :game/expeditions
                :game/moves
                :game/deck]))

(defn start-game
  [players]
  (let [deck (shuffle deck)]
    {:game/turn (rand-nth players)
     :game/players players
     :game/discards empty-stacks
     :game/hands {:mike (take 8 deck)
                  :abby (take 8 (drop 8 deck))}
     :game/deck (drop 16 deck)
     :game/moves []
     :game/expeditions {:mike empty-stacks
                        :abby empty-stacks}}))

(s/fdef start-game
  :args (s/cat :foo (s/spec :game/players))
  :ret :game/state)

(stest/instrument `start-game)


(defn right-player?
  [state move]
  (= (:game/turn state) (:game/player move)))

(defn validate-placement
  [{:keys [:game/hands :game/expeditions]} {:keys [:game/player :game/card :move/destination]}]
  (if-not (some #(= % card) (get hands player))
    :not-in-hand
    (when (= destination :expedition)
      (let [{:keys [:card/type :card/color :card/number]} card
            expedition (get-in expeditions [player color])
            top-card (last expedition)]
        (when-not (or (nil? top-card) (= (:card/type top-card) :wager))
          (if (= type :wager)
            :expedition-underway
            (when-not (> number (:card/number top-card))
              :too-low)))))))

(defn validate-draw
  [state move]
  (let [source (:move/source move)]
    (when (and (not= (:move/source move) :deck)
               (empty? (get-in state [:game/discards state])))
      :discard-empty)))

(defn card-in-hand?
  [state move]
  (let [hands (:game/hands state)
        player (:game/player move)
        card (:game/card move)]
    (some #(= % card) (get hands player))))

(defn remove-first
  [coll item]
  (let [[before from] (split-with (partial not= item) coll)]
    (concat before (rest from))))

(defn draw-card
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
  [state player card]
  (update-in state [:game/hands player] #(remove-first % card)))

(defn add-to-hand
  [state player card]
  (update-in state [:game/hands player] #(conj % card)))

(defn discard-card
  [state card]
  (update-in state [:game/discards (:card/color card)] #(conj % card)))

(defn play-card
  [state player card]
  (update-in state [:game/expeditions player (:card/color card)] #(conj % card)))

(defn make-move
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
     :hands (into {} (map (fn [[player hand]] [player (map str-card hand)]) hands))
     }))

(str-state (start-game [:Mike :Abby]))
