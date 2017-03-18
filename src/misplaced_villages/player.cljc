(ns misplaced-villages.player
  #?(:clj
     (:require [clojure.spec :as s]
               [misplaced-villages.card :as card])
     :cljs
     (:require [cljs.spec :as s]
               [misplaced-villages.card :as card])))

(s/def ::id string?)

(defn uniform-color?
  [expedition]
  (let [distinct-colors (set (map ::card/color expedition))]
    (<= 0 (count distinct-colors) 1)))

(s/fdef uniform-color?
  :args (s/cat :cards (s/coll-of ::card/card))
  :ret boolean?)

(defn under-wager-limit?
  [cards]
  (<= (count (filter card/wager? cards)) 3))

(s/fdef under-wager-limit?
  :args (s/cat :cards (s/coll-of ::card/card))
  :ret boolean?)

(defn numbers-in-order?
  [cards]
  ;; Yuck!
  (let [numbers (filter identity
                        (map ::card/number cards))]
    (if (empty? numbers)
      true
      (apply <= numbers)))
  1)

(s/fdef numbers-in-order?
  :args (s/cat :cards (s/coll-of ::card/card))
  :ret boolean?)

(defn wagers-before-numbers?
  [cards]
  (not
   (loop [[head & tail] cards
          found-number? false]
     (when head
       (let [number? (card/number? head)]
         (if (and (not number?) found-number?)
           true
           (recur tail (or found-number? number?))))))))

(s/fdef wagers-before-numbers?
  :args (s/cat :cards (s/coll-of ::card/card))
  :ret boolean?)

(s/def ::expedition (s/and (s/coll-of ::card/card)
                           uniform-color?
                           under-wager-limit?
                           wagers-before-numbers?
                           numbers-in-order?))

(s/def ::expeditions (s/and ::card/color-piles))
(s/def ::opponent-expeditions (s/map-of ::card/color (s/spec ::expedition)))
(s/def ::hand (s/coll-of ::card/card))
(s/def ::data (s/keys :req [::hand
                            ::expeditions]))
