(ns game-test.cards.hardware.mind-s-eye
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mind-s-eye
  ;; Mind's Eye - Gain power tokens on R&D runs, and for 3 tokens and a click, access the top card of R&D
  (testing "Interaction with RDI + Aeneas"
    (do-game
      (new-game {:corp {:deck [(qty "Jackson Howard" 2)]}
                 :runner {:deck ["Mind's Eye" "R&D Interface" "Aeneas Informant"]}})
      (dotimes [_ 2]
        (core/move state :corp (find-card "Jackson Howard" (:hand (get-corp))) :deck))
      (take-credits state :corp)
      (core/gain state :runner :credit 10 :click 20)
      (play-from-hand state :runner "Mind's Eye")
      (let [eye (get-hardware state 0)]
        (is (= 0 (get-counters (refresh eye) :power)) "0 counters on install")
        (dotimes [_ 3]
          (run-empty-server state :rd)
          (click-prompt state :runner "No action"))
        (is (= 3 (get-counters (refresh eye) :power)) "3 counters after 3 runs")
        (play-from-hand state :runner "R&D Interface")
        (play-from-hand state :runner "Aeneas Informant")
        (card-ability state :runner (refresh eye) 0)
        (let [num-creds (:credit (get-runner))]
          (dotimes [_ 2]
            (click-prompt state :runner "Card from deck")
            (click-prompt state :runner "No action")
            (click-prompt state :runner "Yes")) ;Aeneas
          (is (= (+ num-creds 2) (:credit (get-runner))) "Runner has gained 2 from Aeneas"))))))
