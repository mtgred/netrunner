(ns game-test.cards.agendas.the-cleaners
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-cleaners
  ;; The Cleaners
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["The Cleaners" "Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-and-score state "The Cleaners")
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "5 damage dealt to Runner")))
  (testing "No bonus damage when runner 'suffers' damage, ie Cybernetics"
    (do-game
      (new-game {:corp {:deck ["The Cleaners"]}
                 :runner {:deck [(qty "Respirocytes" 3)]}})
      (play-and-score state "The Cleaners")
      (take-credits state :corp)
      (play-from-hand state :runner "Respirocytes")
      (is (= 1 (count (:hand (get-runner)))) "Only 1 damage dealt to Runner from Cybernetics"))))
