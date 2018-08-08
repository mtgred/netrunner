(ns game-test.cards.agendas.reeducation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reeducation
  ;; Reeducation
  (testing "Simple test"
    (do-game
      (new-game {:corp {:deck ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"]}
                 :runner {:deck ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]}})
      (starting-hand state :corp ["Reeducation" "Sweeps Week"])
      (starting-hand state :runner ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? state :runner :waiting) "Runner has wait prompt")
      (is (= 1 (count (get-in @state [:corp :hand]))))
      (is (= 1 (count (get-in @state [:runner :hand]))))
      (click-prompt state :corp (find-card "Sweeps Week" (:hand (get-corp)))) ; put Sweeps Week at bottom of R&D
      (click-prompt state :corp "Done") ; finished selecting cards
      (click-prompt state :corp "Done") ; corp prompt for Done/Start Over
      (is (= "Sweeps Week" (:title (last (:deck (get-corp))))))
      (is (= "Self-modifying Code" (:title (last (:deck (get-runner))))))
      (is (= 1 (count (get-in @state [:corp :hand]))))
      (is (zero? (count (get-in @state [:runner :hand]))))))
  (testing "Extra cards"
    ;; If Corp is adding more cards in HQ than Runner has in their Grip, Runner
    ;; is not 'able' to resolve the effect and doesn't have to add to bottom of Stack
    (do-game
      (new-game {:corp {:deck ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"]}
                 :runner {:deck ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]}})
      (starting-hand state :corp ["Reeducation" "Sweeps Week" "Hedge Fund"])
      (starting-hand state :runner ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? state :runner :waiting) "Runner has wait prompt")
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 1 (count (:hand (get-runner)))))
      (click-prompt state :corp (find-card "Sweeps Week" (:hand (get-corp))))
      (click-prompt state :corp (find-card "Hedge Fund" (:hand (get-corp)))) ; this is the bottom card of R&D
      (click-prompt state :corp "Done") ; finished selecting cards
      (click-prompt state :corp "Done") ; corp prompt for Done/Start Over
      (is (= "Hedge Fund" (:title (last (:deck (get-corp))))))
      (is (= "Sweeps Week" (:title (last (butlast (:deck (get-corp)))))))
      (is (= "Self-modifying Code" (:title (first (:hand (get-runner))))))
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 1 (count (:hand (get-runner))))))))
