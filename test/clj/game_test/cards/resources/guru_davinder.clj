(ns game-test.cards.resources.guru-davinder
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest guru-davinder
  ;; Guru Davinder - no prompt/trash for 'preventing' 0 damage
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Punitive Counterstrike"]}
                 :runner {:deck ["Guru Davinder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Guru Davinder")
      (take-credits state :runner)
      (play-from-hand state :corp "Punitive Counterstrike")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (empty? (get-in @state [:runner :prompt]))
          "There is no prompt for 0 damage")))
  (testing "cannot steal Obokata while installed"
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Obokata Protocol" 10)]}
                 :runner {:deck ["Guru Davinder" (qty "Sure Gamble" 4)]}})
      (play-from-hand state :corp "Obokata Protocol" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :agenda-point 6)
      (play-from-hand state :runner "Guru Davinder")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-runner)))) "Runner did not pay damage")
      (is (not= :runner (:winner @state)) "Runner has not won"))))
