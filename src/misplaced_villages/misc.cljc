(ns misplaced-villages.misc
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [misplaced-villages.card :as card]
   [misplaced-villages.player :as player]
   [misplaced-villages.move :as move]))

(defn cartesian-product
  "See clojure.math.combinatorics/cartesian-product."
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn cycle-after
  "Returns the item from the cycle of coll after the first instance of value."
  [coll value]
  (loop [[head & tail] coll]
    (when head
      (if (= head value)
        (first (or tail coll))
        (recur tail)))))

(defn remove-first
  "Removes the first instance of item from coll."
  [coll item]
  (let [[before from] (split-with (partial not= item) coll)]
    (concat before (rest from))))
