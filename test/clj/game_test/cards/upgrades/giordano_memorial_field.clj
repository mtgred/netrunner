(ns game-test.cards.upgrades.giordano-memorial-field
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest giordano-memorial-field
  ;; Giordano Memorial Field
  (do-game
    (new-game {:corp {:deck ["Giordano Memorial Field" "Hostile Takeover"]}
               :runner {:deck [(qty "Fan Site" 3)]}})
    (play-from-hand state :corp "Giordano Memorial Field" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Fan Site")
    (play-from-hand state :runner "Fan Site")
    (play-from-hand state :runner "Fan Site")
    (take-credits state :runner)
    (play-and-score state "Hostile Takeover")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (let [credits (:credit (get-runner))]
      (click-prompt state :runner "Pay 6 [Credits]")
      (is (= (- credits 6) (:credit (get-runner))) "Runner pays 6 credits to not end the run"))
    (click-prompt state :runner "No action")
    (run-empty-server state "Server 1")
    (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner should only get 1 choice")
    (is (= "End the run" (-> (get-runner) :prompt first :choices first)) "Only choice should be End the run")
    (click-prompt state :runner "End the run")
    (is (not (:run @state)) "Run should be ended from Giordano Memorial Field ability")))
