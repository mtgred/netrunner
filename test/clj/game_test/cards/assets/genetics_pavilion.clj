(ns game-test.cards.assets.genetics-pavilion
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest genetics-pavilion
  ;; Genetics Pavilion - Limit Runner to 2 draws per turn, but only during Runner's turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Genetics Pavilion"]}
                 :runner {:deck ["Diesel" (qty "Sure Gamble" 3) "Sports Hopper"]}})
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :corp)
        (core/rez state :corp gp)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (play-from-hand state :runner "Sports Hopper")
        (play-from-hand state :runner "Diesel")
        (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
        (take-credits state :runner)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (let [hopper (get-hardware state 0)]
          (card-ability state :runner hopper 0)
          (is (= 3 (count (:hand (get-runner)))) "Able to draw 3 cards during Corp's turn")
          (core/derez state :corp (refresh gp))
          (take-credits state :corp)
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/move state :runner (find-card "Diesel" (:discard (get-runner))) :hand)
          (is (= 1 (count (:hand (get-runner)))))
          (play-from-hand state :runner "Diesel")
          (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards with Diesel")
          (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
          (core/rez state :corp (refresh gp))
          (core/draw state :runner)
          (is (= 2 (count (:hand (get-runner)))) "No card drawn; GP counts cards drawn prior to rez")))))
  (testing "vs Fisk Investment Seminar"
    (do-game
      (new-game {:corp {:deck ["Genetics Pavilion" (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Fisk Investment Seminar" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :corp)
        (core/rez state :corp gp)
        (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (is (= 1 (count (:hand (get-runner)))))
        (is (zero? (count (:hand (get-corp)))))
        (play-from-hand state :runner "Fisk Investment Seminar")
        (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
        (is (= 3 (count (:hand (get-corp)))) "Drew all 3 cards"))))
  (testing "Mr. Li interaction. #1594"
    (do-game
      (new-game {:corp {:deck ["Genetics Pavilion"]}
                 :runner {:deck ["Mr. Li" "Account Siphon" "Faerie"
                                 "Sure Gamble" "John Masanori" "Desperado"]}})
      (starting-hand state :runner ["Mr. Li"])
      (play-from-hand state :corp "Genetics Pavilion" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Mr. Li")
      (let [mrli (get-resource state 0)]
        (is (zero? (count (:hand (get-runner)))))
        ; use Mr. Li with 2 draws allowed
        (card-ability state :runner mrli 0)
        (is (= 2 (count (:hand (get-runner)))))
        (click-card state :runner (first (:hand (get-runner))))
        (is (= 1 (count (:hand (get-runner)))))
        ; use Mr. Li with 0 draws allowed
        (card-ability state :runner mrli 0)
        (is (= 1 (count (:hand (get-runner)))))
        (click-card state :runner (first (:hand (get-runner)))) ; will fail because not a valid target
        (click-prompt state :runner "Done") ; cancel out
        (take-credits state :runner)
        (take-credits state :corp)
        (core/draw state :runner)
        (is (= 2 (count (:hand (get-runner)))))
        ; use Mr. Li with 1 draw allowed
        (card-ability state :runner mrli 0)
        (is (= 3 (count (:hand (get-runner)))))
        (click-card state :runner (first (:hand (get-runner)))) ; will fail
        (click-card state :runner (second (:hand (get-runner)))) ; will fail
        (click-card state :runner (second (rest (:hand (get-runner)))))
        (is (= 2 (count (:hand (get-runner)))))))))
