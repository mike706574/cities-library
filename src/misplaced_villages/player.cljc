(ns misplaced-villages.player
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [misplaced-villages.card :as card]))

(s/def ::id string?)
(s/def ::expedition (s/and (s/coll-of ::card/card)
                                 (fn [expedition]
                                   (let [distinct-colors (set (map ::card/color expedition))]
                                     (<= 0 (count distinct-colors) 1)))))

(s/def ::expeditions (s/map-of ::card/color (s/spec ::expedition)))
(s/def ::opponent-expeditions (s/map-of ::card/color (s/spec ::expedition)))
(s/def ::hand (s/coll-of ::card/card))
(s/def ::data (s/keys :req [::hand
                            ::expeditions]))
