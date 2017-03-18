(ns misplaced-villages.report
  (:require
   #?(:clj [clojure.spec :as s]
      :cljs [cljs.spec :as s])
   [clojure.string :as str]
   [misplaced-villages.card :as card]
   [misplaced-villages.player :as player]
   [misplaced-villages.move :as move]
   [misplaced-villages.game :as game]))

(defn str-card
  "Builds a string representation of a card."
  [{:keys [::card/type ::card/color ::card/number]}]
  (str (name color) "-" (or number "wager")))

(defn str-hand
  [hand]
  (doall (map str-card hand)))



(defn str-game
  "Builds a string representation of the game state."
  [state]
  (let [{rounds ::game/rounds} state
        round-number (count rounds)
        round (last rounds)]
    (str/join "\n" [(str "Round #" round-number)
                    ""
                    (str-round round)])))
