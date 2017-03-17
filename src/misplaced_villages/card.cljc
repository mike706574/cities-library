(ns misplaced-villages.card
  (:refer-clojure :exclude [number?])
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])))

(def colors #{:green :red :blue :white :yellow})
(def color? colors)
(def type? #{:wager :number})
(def number? #{2 3 4 5 6 7 8 9 10})

(s/def ::type type?)
(s/def ::color color?)
(s/def ::number number?)

(s/def ::wager-card (s/keys :req [::type ::color]))
(s/def ::number-card (s/keys :req [::type ::color ::number]))

(defmulti card-type ::type)
(defmethod card-type :wager [_] (s/keys :req [::type ::color]))
(defmethod card-type :number [_] (s/keys :req [::type ::color ::number]))

(s/def ::card (s/multi-spec card-type :card/type))

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
  (concat
   (take 3 (repeat (wager color)))
   (map (partial number color) (range 2 11))))

(def deck (reduce concat (map for-color colors)))
(def empty-piles (into {} (map #(vector % []) colors)))
