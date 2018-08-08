(ns game-test.cards.events.rumor-mill
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rumor-mill
  ;; Rumor Mill - interactions with rez effects, additional costs, general event handlers, and trash-effects
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck [(qty "Project Atlas" 2)
                               "Caprice Nisei" "Chairman Hiro" "Cybernetics Court"
                               "Elizabeth Mills" "Ibrahim Salem"
                               "Housekeeping" "Director Haas" "Oberth Protocol"]}
                 :runner {:deck ["Rumor Mill"]}})
      (core/gain state :corp :credit 100 :click 100 :bad-publicity 1)
      (core/draw state :corp 100)
      (play-from-hand state :corp "Caprice Nisei" "New remote")
      (play-from-hand state :corp "Chairman Hiro" "New remote")
      (play-from-hand state :corp "Cybernetics Court" "New remote")
      (play-from-hand state :corp "Elizabeth Mills" "New remote")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Ibrahim Salem" "New remote")
      (play-from-hand state :corp "Oberth Protocol" "New remote")
      (core/move state :corp (find-card "Director Haas" (:hand (get-corp))) :deck)
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (score-agenda state :corp (get-content state :remote5 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 100 :click 100)
      (is (= 4 (get-in (get-corp) [:hand-size :mod])) "Corp has +4 hand size")
      (is (= -2 (get-in (get-runner) [:hand-size :mod])) "Runner has -2 hand size")
      (play-from-hand state :runner "Rumor Mill")
      ;; Additional costs to rez should NOT be applied
      (core/rez state :corp (get-content state :remote6 0))
      (is (= 1 (count (:scored (get-corp)))) "No agenda was auto-forfeit to rez Ibrahim Salem")
      ;; In-play effects
      (is (zero? (get-in (get-corp) [:hand-size :mod])) "Corp has original hand size")
      (is (zero? (get-in (get-runner) [:hand-size :mod])) "Runner has original hand size")
      ;; "When you rez" effects should not apply
      (core/rez state :corp (get-content state :remote4 0))
      (is (= 1 (:bad-publicity (get-corp))) "Corp still has 1 bad publicity")
      ;; Run events (Caprice)
      ;; Make sure Rumor Mill applies even if card is rezzed after RM is put in play.
      (core/rez state :corp (get-content state :remote1 0))
      (run-on state :remote1)
      (run-continue state)
      (is (empty? (:prompt (get-corp))) "Caprice prompt is not showing")
      (run-jack-out state)
      ;; Trashable execs
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (empty? (:scored (get-runner))) "Chairman Hiro not added to runner's score area")
      (run-jack-out state)
      (run-on state "R&D")
      (run-successful state)
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (empty? (:scored (get-runner))) "Director Haas not added to runner's score area")
      (take-credits state :runner)
      ;; Trash RM, make sure everything works again
      (play-from-hand state :corp "Housekeeping")
      (is (= 4 (get-in (get-corp) [:hand-size :mod])) "Corp has +4 hand size")
      (is (zero? (get-in (get-runner) [:hand-size :mod])) "Runner has +0 hand size")
      ;; Additional costs to rez should now be applied again
      (core/rez state :corp (get-content state :remote7 0))
      (click-card state :corp (get-in (get-corp) [:scored 0]))
      (is (zero? (count (:scored (get-corp)))) "Agenda was auto-forfeit to rez Oberth")
      (core/derez state :corp (get-content state :remote4 0))
      (core/rez state :corp (get-content state :remote4 0))
      (is (zero? (:bad-publicity (get-corp))) "Corp has 0 bad publicity")
      (card-ability state :corp (get-content state :remote4 0) 0) ; Elizabeth Mills, should show a prompt
      (is (:prompt (get-corp)) "Elizabeth Mills ability allowed")))
  (testing "Make sure Rumor Mill is not active when hosted on Peddler"
    (do-game
      (new-game {:corp {:deck ["Jeeves Model Bioroids"]}
                 :runner {:deck ["Street Peddler" (qty "Rumor Mill" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (take-credits state :runner)
      (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
      (let [jeeves (get-content state :remote1 0)]
        (core/rez state :corp jeeves)
        (card-ability state :corp jeeves 0)
        (is (= 3 (:click (get-corp))) "Corp has 3 clicks - Jeeves working ok")))))
