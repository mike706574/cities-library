(ns milo.data
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [milo.card :as card]
   [milo.player :as player]
   [milo.misc :as misc]
   [milo.move :as move]
   [milo.game :as game]
   [milo.score :as score]))

(defn potential-moves
  "Returns all moves that would be possible without factoring in expedition and discard pile states."
  [round]
  (let [turn (::game/turn round)
        hand (get-in round [::game/player-data turn ::player/hand])
        combos (misc/cartesian-product hand move/destinations move/sources)]
    (map #(apply move/move turn %) combos)))

(defn possible-moves
  "Returns all moves that are possible when factoring in expedition and discard pile states."
  [round]
  (let [potential-moves (potential-moves round)
        possible-moves (filter
                        (fn [potential-move]
                          (let [issue (game/validate-move round potential-move)]
                            (nil? issue)))
                        potential-moves)]
    possible-moves))

(defn print-discards
  [state]
  (doseq [color card/colors]
    (println (str (name color) " discards: " (pr-str (map card/str-card (get-in state [::game/round ::game/discard-piles color])))))))

(defn print-hands
  [state]
  (doseq [player (::game/players state)]
    (println (str player "'s hand: " (pr-str (map card/str-card (get-in state [::game/round ::game/player-data player ::player/hand])))))))

(defn print-cards-left
  [state]
  (println (str (count (::game/draw-pile (::game/round state))) " cards left. "
                (count (::game/remaining-rounds state)) " rounds left.")))

(defn print-all
  [state]
  (print-cards-left state)
  (print-hands state)
  (print-discards state))
