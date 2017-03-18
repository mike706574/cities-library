(ns misplaced-villages.card
  (:refer-clojure :exclude [number?])
  #?(:clj
     (:require [clojure.spec :as s])
     :cljs
     (:require [cljs.spec :as s])))

(def colors #{:blue :green :red :white :yellow})

(s/def ::type #{:wager :number})
(s/def ::color colors)
(s/def ::number (set (range 2 11)))

(s/def ::wager-card (s/keys :req [::type ::color]))
(s/def ::number-card (s/keys :req [::type ::color ::number]))

(defmulti card-type ::type)
(defmethod card-type :wager [_] (s/keys :req [::type ::color]))
(defmethod card-type :number [_] (s/keys :req [::type ::color ::number]))

(s/def ::card (s/multi-spec card-type :card/type))

(defn wager? [card] (= (::type card) :wager))

(s/fdef wager?
  :args (s/cat :card ::card)
  :ret boolean?)

(defn number? [card] (= (::type card) :number))

(s/fdef number?
  :args (s/cat :card ::card)
  :ret boolean?)

(defn number
  [color number]
  {::type :number ::color color ::number number})

(s/fdef number
  :args (s/cat :color ::color :number ::number)
  :ret ::number-card)

(defn wager
  [color]
  {::type :wager ::color color})

(s/fdef wager
  :args (s/cat :color ::color)
  :ret ::card)

(defn card
  [{type :type color :color num :number}]
  (case type
    :wager (wager color)
    :number (number color num)))

(defn for-color
  [color]
  (set
   (concat
    (take 3 (repeat (wager color)))
    (map (partial number color) (range 2 11)))))

(s/def ::blue (s/coll-of (for-color :blue)))
(s/def ::green (s/coll-of (for-color :green)))
(s/def ::red (s/coll-of (for-color :red)))
(s/def ::white (s/coll-of (for-color :white)))
(s/def ::yellow (s/coll-of (for-color :yellow)))

(s/def ::pile (s/coll-of ::card))

(s/def ::color-piles (s/keys :req-un [::blue ::green ::red ::white ::yellow]))

(def deck (set (reduce concat (map for-color colors))))
(def empty-piles (into {} (map #(vector % []) colors)))

(defn combine-piles
  [piles]
  (reduce
   (fn [cards [color pile]]
     (apply conj cards pile))
   []
   (select-keys piles colors)))

(s/fdef combine-piles
  :args (s/cat :pile ::piles)
  :ret ::pile)

(defn str-card
  "Builds a string representation of a card."
  [{:keys [::color ::number]}]
  (str (name color) "-" (or number "wager")))

(s/fdef str-card
  :args (s/cat :card ::card)
  :ret string?)
