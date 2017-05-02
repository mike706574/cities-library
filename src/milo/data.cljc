(ns milo.data
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [milo.card :as card]
   [milo.player :as player]
   [milo.misc :as misc]
   [milo.game :as game]
   [milo.score :as score]))

(defn potential-moves
  "Returns all moves that would be possible without factoring in expedition and
  discard pile games."
  [round]
  (let [turn (::game/turn round)
        hand (get-in round [::game/player-data turn ::player/hand])
        combos (misc/cartesian-product hand game/destinations game/sources)]
    (map #(apply game/move turn %) combos)))

(defn possible-moves
  "Returns all moves that are possible when factoring in expedition and discard
  pile games."
  [round]
  (let [potential-moves (potential-moves round)
        possible-moves (filter
                        (fn [potential-move]
                          (let [issue (game/validate-move round potential-move)]
                            (nil? issue)))
                        potential-moves)]
    possible-moves))

(defn print-discards
  [game]
  (doseq [color card/colors]
    (println (str (name color) " discards: " (pr-str (map card/label (get-in game [::game/round ::game/discard-piles color])))))))

(defn print-hands
  [game]
  (doseq [player (::game/players game)]
    (println (str player "'s hand: " (pr-str (map card/label (get-in game [::game/round ::game/player-data player ::player/hand])))))))

(defn print-cards-left
  [game]
  (println (str (count (::game/draw-pile (::game/round game))) " cards left. "
                (count (::game/remaining-rounds game)) " rounds left.")))

(defn print-all
  [game]
  (print-cards-left game)
  (print-hands game)
  (print-discards game))
