(ns game-test.cards.programs.equivocation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game {:corp {:deck [(qty "Restructure" 3) (qty "Hedge Fund" 3)]}
               :runner {:id "Laramy Fisk: Savvy Investor"
                        :deck ["Equivocation" "Desperado"]}})
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (play-from-hand state :runner "Equivocation")
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :rd)
    (click-prompt state :runner "Laramy Fisk: Savvy Investor")
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:hand (get-corp)))) "Corp forced to draw by Fisk")
    (click-prompt state :runner "Yes") ; Equivocation prompt
    (click-prompt state :runner "Yes") ; force the draw
    (is (= 1 (:credit (get-runner))) "Runner gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-corp)))) "Corp forced to draw by Equivocation")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))
