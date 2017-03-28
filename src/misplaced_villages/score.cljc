(ns misplaced-villages.score
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [misplaced-villages.card :as card]
   [misplaced-villages.player :as player]
   [misplaced-villages.game :as game]
   [misplaced-villages.misc :as misc]))

(defn expedition-score
  "Calculates the score for an expedition."
  [expedition]
  (if (empty? expedition)
    0
    (let [wage-count (count (filter card/wager? expedition))
          wage-factor (inc wage-count)
          numbers (map ::card/number (filter card/number? expedition))
          sum (reduce + numbers)
          bonus (if (> (count expedition) 7) 20 0)]
      (+ (* wage-factor sum)
         -20
         bonus))))

(s/fdef expedition-score
  :args (s/cat :expedition ::card/pile)
  :ret integer?)

(defn round-score
  "Calculates a player's score for a round."
  [player-data]
  (let [expeditions (::player/expeditions player-data)
        scores (map #(expedition-score (val %)) expeditions)]
    (reduce + scores)))

(defn round-scores
  [round]
  (misc/fmap
   round-score
   (::game/player-data round)))
