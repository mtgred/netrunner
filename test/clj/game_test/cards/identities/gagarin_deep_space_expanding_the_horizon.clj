(ns game-test.cards.identities.gagarin-deep-space-expanding-the-horizon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gagarin-deep-space-expanding-the-horizon
  ;; Gagarin - pay 1c to access each card in remote
  (do-game
    (new-game {:corp {:id "Gagarin Deep Space: Expanding the Horizon"
                      :deck ["PAD Campaign" "Caprice Nisei"]}})
    (core/lose state :runner :credit 4)
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-card state :runner (get-content state :remote1 0))
    (is (zero? (:credit (get-runner))) "Paid 1 credit to access")
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :remote1)
    (click-card state :runner (get-content state :remote1 0))
    (click-prompt state :runner "OK") ; Could not afford message dismissed
    (is (empty? (:prompt (get-runner))) "Runner cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprice") "Accessed card name was logged")))
