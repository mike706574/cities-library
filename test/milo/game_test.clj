(ns milo.game-test
  (:require
   [clojure.edn :as edn]
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [clojure.test :refer :all]
   [clojure.test.check :as tc]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [milo.card :as card]
   [milo.player :as player]
   [milo.game :as game]
   [clojure.test :as test]
   [milo.data :as data]))

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

(stest/check 'milo.game/collect-cards)

(def players #{"Mike" "Abby"})

(defn valid-player-gen []
  (s/gen (s/and ::player/id players)))

(def test-game (game/rand-game players))

(stest/check 'milo.game/round
  {:gen {:milo.player/id #(s/gen players)}})

(def steps (atom []))

(defn simulate-game-with-invalid-moves
  [moves]
  (loop [response {::game/status :start
                   ::game/game test-game}
         [move & remaining-moves] moves]
    (if move
      (let [response (game/take-turn (::game/game response) move)]
        (swap! steps conj {:move move :status (::game/status response)})
        (recur response remaining-moves))
      response)))

(s/fdef simulate-game-with-invalid-moves
  :args (s/cat :moves (s/coll-of ::game/move :min-count 300))
  :ret ::game/response)

(comment
  (tc/quick-check
   1
   (prop/for-all [moves (s/gen (s/coll-of ::game/move :min-count 1000)
                               {::player/id #(s/gen players)})]
     (let [{:keys [::game/status ::game/game]} (simulate-game moves)]
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
                 ::game/game]} {::game/status :initial
                                 ::game/game test-game}]
    (data/print-cards-left game)
    (cond
      (= 1000 i) :timeout
      (= status :game-over) {:status status
                             :final-game game
                             :moves moves}
      (= status :card-not-in-hand) {:status status
                                    :game game
                                    :moves moves}
      :else (let [possible-moves (data/possible-moves (::game/round game))
                  move (rand-nth possible-moves)
                  _ (println (game/str-move move))
                  game (game/take-turn game move)]
              (recur (inc i) (conj moves move) game)))))
