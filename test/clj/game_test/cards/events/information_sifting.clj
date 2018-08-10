(ns game-test.cards.events.information-sifting
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest information-sifting
  ;; Information Sifting - complicated interactions with damage prevention
  (do-game
    (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                      :deck ["Snare!" "PAD Campaign" "Hostile Infrastructure"
                             "Braintrust" "Hedge Fund" "Power Shutdown"]}
               :runner {:deck [(qty "Information Sifting" 2) (qty "Deus X" 2) "Sure Gamble"]}})
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (core/gain state :corp :credit 10)
    (core/rez state :corp (get-content state :remote1 0))
    (core/gain state :runner :credit 10)
    (take-credits state :corp)
    (play-from-hand state :runner "Deus X")
    (play-from-hand state :runner "Deus X")
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (click-card state :corp (find-card "Snare!" (:hand (get-corp))))
    (click-card state :corp (find-card "PAD Campaign" (:hand (get-corp))))
    (click-prompt state :corp "Done")
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Corp is waiting for Runner selection")
    (click-prompt state :runner "Pile 1 (2 cards)")
    (click-prompt state :runner "Card from pile 1")
    ;; the cards are selected randomly :(
    (letfn [(prevent-snare [existing-dmg]
              (click-prompt state :corp "Yes")
              (card-ability state :runner (get-program state 0) 1)
              (click-prompt state :runner "Done")
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Damage from Snare! prevented")
              (click-prompt state :runner (-> (prompt-map :runner) :choices first))
              (when (-> (prompt-map :runner) :choices first)
                (click-prompt state :runner "Done")) ; don't prevent Hostile dmg
              ;; chronos prompt
              (click-prompt state :corp "Yes")
              (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner))))
              (is (= (+ 2 existing-dmg) (count (:discard (get-runner)))) "Damage from Hostile Inf not prevented"))
            (allow-pad [existing-dmg]
              (click-prompt state :runner (-> (prompt-map :runner) :choices first))
              (card-ability state :runner (get-program state 0) 1)
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Runner prevented damage from Hostile Inf")
              (click-prompt state :runner "Done"))]
      (if (= :waiting (-> (get-runner) :prompt first :prompt-type)) ; hit the snare
        ;; prevent the damage
        (do (prevent-snare (count (:discard (get-runner))))
            (click-prompt state :runner "Card from pile 1")
            (allow-pad (count (:discard (get-runner)))))
        (do (allow-pad (count (:discard (get-runner))))
            (click-prompt state :runner "Card from pile 1")
            (prevent-snare (count (:discard (get-runner)))))))
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (click-card state :corp (find-card "Power Shutdown" (:hand (get-corp))))
    (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Selecting max cards closed the selection prompt")
    (click-prompt state :runner "Pile 2 (1 card)")
    (click-prompt state :runner "Card from pile 2")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Runner stole agenda")))
