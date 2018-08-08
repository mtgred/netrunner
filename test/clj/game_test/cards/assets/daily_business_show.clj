(ns game-test.cards.assets.daily-business-show
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest daily-business-show
  ;; Daily Business Show
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck [(qty "Daily Business Show" 3) "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"]}})
      (starting-hand state :corp ["Daily Business Show" "Daily Business Show" "Daily Business Show" "Hedge Fund"])
      (core/gain state :corp :credit 1)
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (take-credits state :runner)
      (is (= 5 (count (:hand (get-corp)))) "Drew an additional 3 cards with 3 DBS")
      (is (not-empty (:prompt (get-runner))) "Runner is waiting for Corp to use DBS")
      (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp)))) ;invalid target
      (click-card state :corp (find-card "Resistor" (:hand (get-corp))))
      (click-card state :corp (find-card "Product Placement" (:hand (get-corp))))
      (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
      (is (empty? (:prompt (get-runner))) "Runner prompt cleared")
      (is (= 2 (count (:hand (get-corp)))))
      (is (= "Hedge Fund" (:title (first (:hand (get-corp))))))
      (is (= "Jackson Howard" (:title (second (:hand (get-corp))))))
      (is (= "Resistor" (:title (last (:deck (get-corp))))) "Resistor last card in deck")
      (is (= "Product Placement" (:title (last (butlast (:deck (get-corp))))))
          "Product Placement second last card in deck")
      (is (= "Breaking News" (:title (last (butlast (butlast (:deck (get-corp)))))))
          "Breaking News third last card in deck")))
  (testing "Sensie Actors Union interaction"
    (do-game
      (new-game {:corp {:deck ["Daily Business Show" (qty "Sensie Actors Union" 2)
                               "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"]}})
      (starting-hand state :corp ["Daily Business Show" "Sensie Actors Union" "Sensie Actors Union" "Hedge Fund"])
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (play-from-hand state :corp "Sensie Actors Union" "New remote")
      (play-from-hand state :corp "Sensie Actors Union" "New remote")
      (let [sensie1 (get-content state :remote2 0)
            sensie2 (get-content state :remote3 0)]
        (core/rez state :corp (get-content state :remote1 0))
        (core/rez state :corp sensie1)
        (core/rez state :corp sensie2)
        (take-credits state :corp)
        (take-credits state :runner)
        ;; Use first Sensie
        (is (= 1 (count (:hand (get-corp)))))
        (card-ability state :corp sensie1 0)
        (is (= 5 (count (:hand (get-corp)))) "Drew 3 cards with Sensie, +1 with DBS")
        (click-card state :corp (find-card "Resistor" (:hand (get-corp)))) ; DBS target
        (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp)))) ; Sensie target
        (is (= 3 (count (:hand (get-corp)))))
        (is (= "Hedge Fund" (:title (last (:deck (get-corp))))) "Hedge Fund last card in deck")
        (is (= "Resistor" (:title (last (butlast (:deck (get-corp))))))
            "Resistor second last card in deck")
        ;; Try to use first Sensie again
        (card-ability state :corp sensie1 0)
        (is (empty? (-> @state :corp :prompt)) "Sensie didn't activate")
        (is (= 3 (count (:hand (get-corp)))))
        ;; Use second Sensie
        (starting-hand state :corp ["Hedge Fund" "Jackson Howard"])
        (is (= 2 (count (:hand (get-corp)))))
        (card-ability state :corp sensie2 0)
        (is (= 5 (count (:hand (get-corp)))) "Drew 3 cards with Sensie, DBS didn't activate")
        (click-card state :corp (find-card "Breaking News" (:hand (get-corp)))) ; Sensie target
        (is (= "Breaking News" (:title (last (:deck (get-corp))))) "Breaking News last card in deck"))))
  (testing "Should not trigger if rezzed after mandatory draw"
    (do-game
      (new-game {:corp {:deck [(qty "Daily Business Show" 3) "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"]}})
      (starting-hand state :corp ["Daily Business Show"])
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/draw state :corp)
      (is (= 1 (count (:hand (get-corp)))) "DBS did not fire on manual draw")
      (is (empty? (:prompt (get-corp))) "Corp is not being asked to bury a card with DBS")))
  (testing "Fire on Runner turn"
    (do-game
      (new-game {:corp {:deck ["Daily Business Show" "Hedge Fund"
                               "Resistor" "Product Placement" "Breaking News"]}
                 :runner {:deck ["Fisk Investment Seminar"]}})
      (starting-hand state :corp ["Daily Business Show"])
      (play-from-hand state :corp "Daily Business Show" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (is (empty? (:hand (get-corp))) "Corp hand is empty")
      (play-from-hand state :runner "Fisk Investment Seminar")
      (is (= 4 (count (:hand (get-corp)))) "Drew an additional card from FIS")
      (is (not-empty (:prompt (get-runner))) "Runner is waiting for Corp to use DBS")
      (click-card state :corp (find-card "Resistor" (:hand (get-corp))))
      (is (empty? (:prompt (get-runner))) "Runner prompt cleared")
      (is (= 3 (count (:hand (get-corp))))))))
