(ns misplaced-villages.state-test
  (:require [misplaced-villages.state :as domain]
            [clojure.test :refer :all]))

(def state
  #:game{:turn :bob,
         :players [:bob :alice],
         :discards
         {:white [], :yellow [], :green [], :red [], :blue []},
         :hands
         {:bob
          '(#:card{:type :number, :color :red, :number 7}
                  #:card{:type :number, :color :blue, :number 3}
                  #:card{:type :number, :color :green, :number 9}
                  #:card{:type :number, :color :red, :number 6}
                  #:card{:type :wager, :color :yellow}
                  #:card{:type :number, :color :red, :number 5}
                  #:card{:type :number, :color :white, :number 8}
                  #:card{:type :number, :color :yellow, :number 9}),
          :alice
          '(#:card{:type :wager, :color :blue}
                  #:card{:type :number, :color :blue, :number 10}
                  #:card{:type :number, :color :green, :number 2}
                  #:card{:type :number, :color :green, :number 10}
                  #:card{:type :number, :color :blue, :number 2}
                  #:card{:type :number, :color :blue, :number 7}
                  #:card{:type :wager, :color :blue}
                  #:card{:type :wager, :color :red})},
         :deck
         '(#:card{:type :number, :color :white, :number 5}
                 #:card{:type :number, :color :yellow, :number 5}
                 #:card{:type :number, :color :blue, :number 4}
                 #:card{:type :number, :color :blue, :number 5}
                 #:card{:type :number, :color :green, :number 7}
                 #:card{:type :number, :color :white, :number 2}
                 #:card{:type :number, :color :green, :number 5}
                 #:card{:type :number, :color :green, :number 4}
                 #:card{:type :number, :color :yellow, :number 3}
                 #:card{:type :number, :color :yellow, :number 7}
                 #:card{:type :number, :color :green, :number 6}
                 #:card{:type :number, :color :blue, :number 6}
                 #:card{:type :number, :color :yellow, :number 10}
                 #:card{:type :wager, :color :red}
                 #:card{:type :wager, :color :white}
                 #:card{:type :number, :color :green, :number 8}
                 #:card{:type :wager, :color :green}
                 #:card{:type :number, :color :yellow, :number 8}
                 #:card{:type :number, :color :red, :number 9}
                 #:card{:type :number, :color :white, :number 3}
                 #:card{:type :number, :color :red, :number 2}
                 #:card{:type :number, :color :red, :number 8}
                 #:card{:type :number, :color :white, :number 10}
                 #:card{:type :wager, :color :white}
                 #:card{:type :wager, :color :yellow}
                 #:card{:type :wager, :color :green}
                 #:card{:type :number, :color :yellow, :number 4}
                 #:card{:type :number, :color :yellow, :number 6}
                 #:card{:type :number, :color :red, :number 4}
                 #:card{:type :number, :color :white, :number 6}
                 #:card{:type :wager, :color :white}
                 #:card{:type :number, :color :red, :number 10}
                 #:card{:type :number, :color :white, :number 7}
                 #:card{:type :number, :color :white, :number 9}
                 #:card{:type :number, :color :white, :number 4}
                 #:card{:type :wager, :color :green}
                 #:card{:type :number, :color :blue, :number 8}
                 #:card{:type :number, :color :yellow, :number 2}
                 #:card{:type :number, :color :blue, :number 9}
                 #:card{:type :wager, :color :red}
                 #:card{:type :wager, :color :yellow}
                 #:card{:type :number, :color :green, :number 3}
                 #:card{:type :number, :color :red, :number 3}
                 #:card{:type :wager, :color :blue}),
         :moves [],
         :expeditions
         {:bob {:white [], :yellow [], :green [], :red [], :blue []},
          :alice {:white [], :yellow [], :green [], :red [], :blue []}}})

(deftest logic
  (is (= :wrong-player
         (:control/status (domain/take-turn
                           state
                           {:game/player :alice
                            :move/destination :expedition
                            :move/source :deck
                            :game/card {:card/type :wager
                                        :card/color :blue}}))))
  (is (= :card-not-in-hand
         (:control/status (domain/take-turn
                           state
                           {:game/player :bob
                            :move/destination :expedition
                            :move/source :deck
                            :game/card {:card/type :wager
                                        :card/color :blue}}))))

  (is (= :card-not-in-hand
         (:control/status (domain/take-turn
                           state
                           {:game/player :bob
                            :move/destination :expedition
                            :move/source :deck
                            :game/card {:card/type :wager
                                        :card/color :yellow}})))))

(let [move-1 {:game/player :bob
              :move/destination :expedition
              :move/source :deck
              :game/card {:card/type :wager
                          :card/color :yellow}}
      {:keys [:control/status :game/state]} (-> state
                                                ) ]

  )
