(ns game-test.cards.identities.ayla-bios-rahim-simulant-specialist
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ayla-bios-rahim-simulant-specialist
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game {:runner {:id "Ayla \"Bios\" Rahim: Simulant Specialist"
                        :deck ["Sure Gamble" "Desperado"
                               "Security Testing" "Bank Job"
                               "Heartbeat" "Eater"]}}
              {:dont-start-game true})
    (is (= 6 (count (get-in @state [:runner :play-area]))) "Deck cards are in play area")
    (is (zero? (count (get-in @state [:runner :hand]))))
    (click-card state :runner (find-card "Sure Gamble" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Desperado" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Bank Job" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Eater" (get-in @state [:runner :play-area])))
    (is (= 4 (count (:hosted (:identity (get-runner))))) "4 cards in NVRAM")
    (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
    (click-prompt state :corp "Keep")
    (click-prompt state :runner "Keep")
    (take-credits state :corp)
    (is (= 2 (count (get-in @state [:runner :hand]))) "There are 2 cards in the runner's Grip")
    (card-ability state :runner (:identity (get-runner)) 0)
    (click-prompt state :runner (find-card "Bank Job" (:hosted (:identity (get-runner)))))
    (is (= 3 (count (get-in @state [:runner :hand]))) "There are 3 cards in the runner's Grip")))
