(ns game-test.cards.upgrades.bio-vault
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bio-vault
  ;; Bio Vault - 2 advancement tokens + trash to end the run
  (do-game
    (new-game {:corp {:deck ["Bio Vault"]}})
    (play-from-hand state :corp "Bio Vault" "New remote")
    (take-credits state :corp)
    (let [bv (get-content state :remote1 0)]
      (run-on state "Server 1")
      (core/rez state :corp (refresh bv))
      (card-ability state :corp (refresh bv) 0)
      (is (:run @state) "Bio Vault doesn't fire if less than 2 advancements")
      (run-successful state)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (advance state (refresh bv) 2)
      (take-credits state :corp)
      (run-on state "Server 1")
      (card-ability state :corp (refresh bv) 0)
      (is (not (:run @state)) "Bio Vault fires with 2 advancement tokens")
      (is (= 1 (count (:discard (get-corp)))) "Bio Vault trashed"))))
