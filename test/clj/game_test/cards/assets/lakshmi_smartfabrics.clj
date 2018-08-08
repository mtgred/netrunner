(ns game-test.cards.assets.lakshmi-smartfabrics
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest lakshmi-smartfabrics
  ;; Lakshmi Smartfabrics - Gain power counter when rezzing a card; use counters to protect agenda in HQ
  (do-game
    (new-game {:corp {:deck ["Lakshmi Smartfabrics" "Vanilla"
                             "Marked Accounts" "Elective Upgrade"]}})
    (play-from-hand state :corp "Lakshmi Smartfabrics" "New remote")
    (let [lak (get-content state :remote1 0)]
      (core/rez state :corp lak)
      (is (= 1 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter for itself")
      (play-from-hand state :corp "Vanilla" "R&D")
      (play-from-hand state :corp "Marked Accounts" "New remote")
      (core/rez state :corp (get-ice state :rd 0))
      (is (= 2 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter")
      (core/rez state :corp (get-content state :remote2 0))
      (is (= 3 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter")
      (take-credits state :corp)
      (card-ability state :corp (refresh lak) 0)
      (click-card state :corp (find-card "Elective Upgrade" (:hand (get-corp))))
      (is (last-log-contains? state "Elective Upgrade") "Revealed agenda")
      (is (zero? (get-counters (refresh lak) :power)) "Spent 3 power counters")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (empty? (:scored (get-runner))) "Steal prevented by Smartfabrics")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 3 (:agenda-point (get-runner))) "Runner could steal on later turn"))))
