(ns game-test.cards.icebreakers.persephone
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest persephone
  ;; Persephone's ability trashes cards from R&D, triggering AR-Enhanced Security
  ;; See #3187
  (do-game
    (new-game {:corp {:deck ["Zed 1.0" (qty "Zed 2.0" 3) "AR-Enhanced Security"]}
               :runner {:deck [(qty "Persephone" 10)]}})
    (core/move state :corp (find-card "Zed 2.0" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Zed 2.0" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "AR-Enhanced Security" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Zed 1.0" "Archives")
    (core/rez state :corp (get-ice state :archives 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Persephone")
    (run-on state "Archives")
    (run-continue state)
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "2")
    (is (= 1 (:tag (get-runner))) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")
    (take-credits state :runner)
    ;; Gotta move the discarded cards back to the deck
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "2")
    (is (= 2 (:tag (get-runner))) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")))
