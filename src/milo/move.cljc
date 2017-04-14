(ns milo.move
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [milo.card :as card]
   [milo.player :as player]))

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

(defn exp
  [player-id card source]
  {::player/id player-id
   ::card/card card
   ::destination :expedition
   ::source source})

(s/fdef exp
  :args (s/cat :player-id ::player/id
               :card ::card/card
               :source ::source)
  :ret ::move)

(defn exp*
  [player-id card]
  {::player/id player-id
   ::card/card card
   ::destination :expedition
   ::source :draw-pile})

(s/fdef exp*
  :args (s/cat :player-id ::player/id
               :card ::card/card)
  :ret ::move)

(defn disc
  [player-id card source]
  {::player/id player-id
   ::card/card card
   ::destination :discard-pile
   ::source source})

(s/fdef disc
  :args (s/cat :player-id ::player/id
               :card ::card/card
               :source ::source)
  :ret ::move)

(defn disc*
  [player-id card]
  {::player/id player-id
   ::card/card card
   ::destination :discard-pile
   ::source :draw-pile})

(s/fdef disc*
  :args (s/cat :player-id ::player/id
               :card ::card/card)
  :ret ::move)

(defn str-move
  [{:keys [::player/id ::card/card ::destination ::source]}]
  (str id " "
       (case destination
         :expedition "plays"
         :discard-pile "discards")
       " "
       (card/str-card card)
       ", draws "
       (if (= :draw-pile source)
         "new card."
         (str "from " (name source) " discard pile."))))
