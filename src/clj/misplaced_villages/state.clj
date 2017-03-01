(ns misplaced-villages.state
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.core.match :refer [match]]
            [dire.core :refer [with-pre-hook!]]
            [taoensso.timbre :as log]))

;; cards
(def colors #{:green :red :blue :white :yellow})
(def color? colors)
(def type? #{:wager :number})
(def number? #{2 3 4 5 6 7 8 9 10})

(s/def :card/type type?)

(map #(s/valid? :card/type %) [:zoogaloo :wager :number])

(s/def :card/color color?)

(map #(s/valid? :card/color %) [:orange :green :red :blue :white :yellow])


(s/def :card/number number?)

(map #(s/valid? :card/number %) [0 1 2 3 4 5 6 7 8 9 10 11])

(s/def :game/wager-card (s/keys :req [:card/type :card/color]))
(s/def :game/number-card (s/keys :req [:card/type :card/color :card/number]))

(defmulti card-type :card/type)
(defmethod card-type :wager [_] (s/keys :req [:card/type :card/color]))
(defmethod card-type :number [_] (s/keys :req [:card/type :card/color :card/number]))

(s/valid? :game/wager-card #:card{:type :number
                                  :color :green
                                  :number 3})

(s/def :game/card (s/multi-spec card-type :card/type))

(let [card #:card{}]
  (map #(s/valid? % card) [:game/card :game/wager-card :game/number-card]))

(let [card #:card{:type :wager
                  :color :green}]
  (map #(s/valid? % card) [:game/card :game/wager-card :game/number-card]))

(let [card #:card{:type :wager
                  :color :orange}]
  (map #(s/valid? % card) [:game/card :game/wager-card :game/number-card]))

(let [card #:card{:type :number
                  :color :green
                  :number 100}]
  (map #(s/valid? % card) [:game/card :game/wager-card :game/number-card]))

(let [card #:card{:type :number
                  :color :green
                  :number 3}]
  (map #(s/valid? % card) [:game/card :game/wager-card :game/number-card]))

;; constructing number cards
(defn number-card
  [color number]
  {:card/type :number :card/color color :card/number number})

(s/fdef number-card
  :args (s/cat :color :card/color :number :card/number)
  :ret :game/number-card)

(stest/instrument `number-card)

(comment
  (number-card :orange 0)
  (number-card :green 2)
)

;; constructing wager cards
(defn wager-card
  [color]
  {:card/type :wager :card/color color})

(s/fdef wager-card
  :args (s/cat :color :card/color)
  :ret :game/card)

(stest/instrument `wager-card)

;; more things

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

(s/def :game/discard (s/coll-of :game/card))
(s/def :game/discards (s/map-of :game/player :game/discard))

(s/def :game/expedition (s/coll-of :game/card))
(s/def :game/expeditions (s/map-of :game/player :game/expedition))

(s/def :game/hand (s/coll-of :game/card :count 8))
(s/def :game/hands (s/map-of :game/player :game/hand))

(s/def :game/turn :game/player)

(s/def :game/deck (s/coll-of :game/card))

(s/def :game/state
  (s/keys :req [:game/hands
                :game/turn
                :game/discards
                :game/expeditions
                :game/deck]))

(defn start-game
  [players]
  (let [deck (shuffle deck)]
    #:game{:turn (rand-nth players)
           :discards empty-stacks
           :deck (drop 16 deck)
           :moves []
           :hands {:mike (take 8 deck)
                   :abby (take 8 (drop 8 deck))}
           :expeditions {:mike empty-stacks
                         :abby empty-stacks}}))

(s/fdef start-game
  ;; TODO: Why doesn't this work?
;;  :args (s/cat :players :game/players)
  :ret :game/state)

(stest/instrument `start-game)

(start-game [:bob :alice])



(comment
(defn process-move
  [{:keys [turn hands expeditions]} action card player]
  (cond
    (not= turn player) [:not-your-turn]
    (not (contains? (hands player) card)) [:not-in-hand]
    :else (let [[color type number] card
                expedition (color (expedition player))]
            (if (empty? expedition)
              [:start-expedition color]
              (let [wager? (= type :wager)
                    wage-level (count (filter #(= (:type %) :wager) expedition))
                    [_ last-type last-number] (last expedition)
                    numbered? (= last-type :number)]
                (cond
                  (and wager? numbered?) [:wage-rejection]
                  wager? [:wage (inc wage-level)]
                  (< number ))
                  )

                )

              )
            )

    )
  (if-not (= turn player)
    {:status :wrong-player}
    (let [expeditions (get expeditions player)]
      (if-not (contains? (hands player) (dissoc card :player))


        (let [[color type number] card
              expedition (color (expeditions player))
              [last-color last-type last-number] (last expedition)]
          (if (empty? expedition)
            {:status :played
             :state (make-move state move)}

            ;; play card
            ;; check if valid, error if not, play if valid

            )))))





  [expeditions {:keys [type color number]}]


  (defn take-turn
    [{:keys [turn hands expeditions] :as state} {:keys [player card] :as move}]
    (if-not (= turn player)
      {:status :wrong-player :state state}
      (let [expeditions (get expeditions player)]
        (if-not (contains? (hands player) card)
          {:status :wrong-card :state state}
          (let [[color type number] card
                expedition (color (expeditions player))
                [last-color last-type last-number] (last expedition)]
            (if (empty? expedition)
              {:status :played
               :state (make-move state move)}

              ;; play card
              ;; check if valid, error if not, play if valid

              ))))))

p  (with-pre-hook! #'take-turn
    (fn [state move]
      (log/info "Move:" move)))


  (let [state (assoc (start-game) :turn :mike)]
    (take-turn state [:abby 1 2]))
  )
