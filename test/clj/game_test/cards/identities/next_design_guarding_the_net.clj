(ns game-test.cards.identities.next-design-guarding-the-net
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest next-design-guarding-the-net
  ;; Next Design.  Install up to 3 ICE before game starts, one per server max, and re-draw to 5
  (do-game
    (new-game {:corp {:id "NEXT Design: Guarding the Net"
                      :deck [(qty "Snowflake" 10)]}}
              {:dont-start-turn true})
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "HQ")
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "R&D")
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (= 5 (count (:hand (get-corp)))) "Corp should start with 5 cards in hand")))
