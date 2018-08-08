(ns game-test.cards.ice.otoroshi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest otoroshi
  ;; Otoroshi
  (do-game
    (new-game {:corp {:deck ["Otoroshi" "Project Junebug" (qty "Ice Wall" 100)]}})
    (starting-hand state :corp ["Otoroshi" "Project Junebug"])
    (play-from-hand state :corp "Otoroshi" "New remote")
    (play-from-hand state :corp "Project Junebug" "New remote")
    (take-credits state :corp)
    (run-on state :remote1)
    (let [otoroshi (get-ice state :remote1 0)
          junebug (get-content state :remote2 0)
          credits (:credit (get-runner))]
      (is (zero? (get-counters (refresh junebug) :advancement)) "Project Junebug should start with 0 advancement tokens")
      (core/rez state :corp otoroshi)
      (card-subroutine state :corp otoroshi 0)
      (click-card state :corp junebug)
      (is (= 3 (get-counters (refresh junebug) :advancement)) "Project Junebug should have 3 advancement tokens from Otoroshi subroutine")
      (is (= (list "Access card" "Pay 3 [Credits]") (-> (get-runner) :prompt first :choices)) "Runner should have 2 options")
      (click-prompt state :runner "Pay 3 [Credits]")
      (is (= (- credits 3) (:credit (get-runner))) "Runner should pay 3 credits to Otoroshi subroutine")
      (run-jack-out state)
      (run-on state :remote1)
      (card-subroutine state :corp otoroshi 0)
      (click-card state :corp otoroshi)
      (is (= 3 (get-counters (refresh otoroshi) :advancement)) "Otoroshi should have 3 advancement tokens from Otoroshi subroutine")
      (is (= (list "Access card") (-> (get-runner) :prompt first :choices)) "Runner should have 1 option")
      (click-prompt state :runner "Access card")
      (is (= "You accessed Otoroshi." (-> (get-runner) :prompt first :msg)) "Runner should access Otoroshi even tho it's an ice.")
      (click-prompt state :runner "No action"))))
