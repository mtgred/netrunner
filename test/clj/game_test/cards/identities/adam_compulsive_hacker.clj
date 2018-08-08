(ns game-test.cards.identities.adam-compulsive-hacker
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adam-compulsive-hacker
  ;; Adam
  (testing "Allow runner to choose directives"
    (do-game
      (new-game {:runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}}
                {:dont-start-game true})
      (is (= 4 (count (get-in @state [:runner :play-area]))) "All directives are in the runner's play area")
      (is (zero? (count (get-in @state [:runner :hand]))))
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (is (= 3 (count (get-resource state))) "3 directives were installed")
      (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
      (let [nat (find-card "Neutralize All Threats" (get-resource state))
            sf (find-card "Safety First" (get-resource state))
            abr (find-card "Always Be Running" (get-resource state))]
        (is (and nat sf abr) "The chosen directives were installed"))))
  (testing "Directives should not grant Pālanā credits"
    (do-game
      (new-game {:corp {:id "Pālanā Foods: Sustainable Growth"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}}
                {:dont-start-game true})
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (click-prompt state :corp "Keep")
      (click-prompt state :runner "Keep")
      (core/start-turn state :corp nil)
      (is (= 5 (:credit (get-corp))) "Pālanā does not gain credit from Adam's starting Directives")))
  (testing "Neutralize All Threats interaction with advanceable traps"
    (do-game
      (new-game {:corp {:deck [(qty "Cerebral Overwriter" 3)]}
                 :runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}}
                {:dont-start-game true})
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (click-prompt state :corp "Keep")
      (click-prompt state :runner "Keep")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (advance state (get-content state :remote1 0) 2)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "1 card in archives"))))
