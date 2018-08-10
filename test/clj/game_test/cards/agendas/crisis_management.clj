(ns game-test.cards.agendas.crisis-management
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest crisis-management
  ;; Crisis Management
  (do-game
    (new-game {:corp {:deck ["Crisis Management"]}})
    (play-and-score state "Crisis Management")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "No damage done, Runner not tagged")
    (take-credits state :corp)
    (core/gain state :runner :tag 1)
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-runner)))) "Crisis Management dealt 1 meat damage")))
