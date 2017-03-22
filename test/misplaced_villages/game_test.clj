(ns misplaced-villages.game-test
  (:require
   [clojure.edn :as edn]
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]
   [clojure.test.check :as tc]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [misplaced-villages.card :as card]
   [misplaced-villages.player :as player]
   [misplaced-villages.move :as move]
   [misplaced-villages.game :as game]
   [clojure.test :as test]
   [misplaced-villages.data :as data]))

(stest/instrument)

(deftest determining-if-game-is-over
  (is (game/round-over? {::game/turn "mike"
                         ::game/moves []
                         ::game/discard-piles card/empty-piles
                         ::game/draw-pile []
                         ::game/players {"mike" {::player/hand [(card/wager-1 :blue)]
                                                 ::player/expeditions card/empty-piles}}})
      "If there are no cards in the draw pile, the round is over.")
  (is (not (game/round-over? {::game/turn "mike"
                              ::game/moves []
                              ::game/discard-piles card/empty-piles
                              ::game/draw-pile [(card/wager-1 :blue)]
                              ::game/players {"mike" {::player/hand []
                                                      ::player/expeditions card/empty-piles}}}))
      "If there are cards in the draw pile, the round isn't over."))

(stest/check 'misplaced-villages.game/collect-cards)

(def players #{"Mike" "Abby"})

(defn valid-player-gen []
  (s/gen (s/and ::player/id players)))

(def test-game (game/rand-game players))

(stest/check 'misplaced-villages.game/round
  {:gen {:misplaced-villages.player/id #(s/gen players)}})

(def steps (atom []))

(defn simulate-game-with-invalid-moves
  [moves]
  (loop [response {::game/status :start
                   ::game/state test-game}
         [move & remaining-moves] moves]
    (if move
      (let [response (game/take-turn (::game/state response) move)]
        (swap! steps conj {:move move :status (::game/status response)})
        (recur response remaining-moves))
      response)))

(s/fdef simulate-game-with-invalid-moves
  :args (s/cat :moves (s/coll-of ::move/move :min-count 300))
  :ret ::game/response)

(comment
  (tc/quick-check
   1
   (prop/for-all [moves (s/gen (s/coll-of ::move/move :min-count 1000)
                               {::player/id #(s/gen players)})]
     (let [{:keys [::game/status ::game/state]} (simulate-game moves)]
       (contains? game/statuses status))))
  )

(data/possible-moves (::game/round (game/rand-game ["Mike" "Abby"])))
(stest/instrument)

(def test-game (game/rand-game ["Mike" "Abby"]))

(defn simulate-game-with-valid-moves
  []
  (loop [i 0
         moves []
         {:keys [::game/status
                 ::game/state]} {::game/status :initial
                                 ::game/state test-game}]
    (data/print-cards-left state)
    (cond
      (= 1000 i) :timeout
      (= status :game-over) {:status status
                             :final-state state
                             :moves moves}
      (= status :card-not-in-hand) {:status status
                                    :state state
                                    :moves moves}
      :else (let [possible-moves (data/possible-moves (::game/round state))
                  move (rand-nth possible-moves)
                  _ (println (move/str-move move))
                  state (game/take-turn state move)]
              (recur (inc i) (conj moves move) state)))))
