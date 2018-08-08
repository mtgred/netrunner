(ns game-test.cards.identities.nasir-meidan-cyber-explorer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nasir-meidan-cyber-explorer
  ;; Nasir
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [iwall (get-ice state :hq 0)
            nasir (get-in @state [:runner :identity])]
        (core/rez state :corp iwall)
        (is (= 5 (:credit (get-runner))) "Nasir Ability does not trigger automatically")
        (card-ability state :runner nasir 0)
        (is (= 1 (:credit (get-runner))) "Credits at 1 after Nasir ability trigger"))))
  (testing "with Xanadu"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"
                          :deck ["Xanadu"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (swap! state assoc-in [:runner :credit] 6)
      (play-from-hand state :runner "Xanadu")
      (run-on state "HQ")
      (let [iwall (get-in @state [:corp :servers :hq :ices 0])
            nasir (get-in @state [:runner :identity])]
        (core/rez state :corp iwall)
        (is (= 3 (:credit (get-runner))) "Pay 3 to install Xanadu")
        (card-ability state :runner nasir 0)
        (is (= 2 (:credit (get-runner))) "Gain 1 more credit due to Xanadu")))))
