(ns misplaced-villages.move
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [misplaced-villages.card :as card]
            [misplaced-villages.player :as player]))

;; A move consists of two phases: placing and drawing
(def destinations #{:expedition :discard-pile})
(def sources (conj card/colors :draw-pile))
(s/def ::source sources)
(s/def ::destination destinations)
(s/def ::move (s/keys :req [::player/id
                            ::card/card
                            ::destination
                            ::source]))
(defn move
  [player-id card destination source]
  {::player/id player-id
   ::card/card card
   ::destination destination
   ::source source})

(s/fdef move
  :args (s/cat :player-id ::player/id
               :card ::card/card
               :destination ::destination
               :source ::source)
  :ret ::move)
