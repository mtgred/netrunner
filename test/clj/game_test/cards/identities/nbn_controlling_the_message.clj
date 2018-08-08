(ns game-test.cards.identities.nbn-controlling-the-message
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message
  (testing "Trace to tag Runner when first installed Corp card is trashed"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck [(qty "Launch Campaign" 3)]}
                 :runner {:deck ["Forger"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Forger")
      ; trash from HQ first - #2321
      (run-empty-server state "HQ")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (empty? (:prompt (get-runner))) "Forger can't avoid the tag")
      (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (empty? (:prompt (get-corp))) "No trace chance on 2nd trashed card of turn")))
  (testing "Interaction with Dedicated Response Team"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck ["Launch Campaign" "Dedicated Response Team"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from DRT"))))
