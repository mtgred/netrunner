(ns game-test.cards.agendas.paper-trail
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest paper-trail
  ;; Paper Trail
  (do-game
    (new-game {:corp {:deck ["Paper Trail"]}
               :runner {:deck ["Aeneas Informant" "Bank Job"
                               "Rosetta 2.0" "Magnum Opus"
                               "Astrolabe"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 10 :credit 10)
    (play-from-hand state :runner "Aeneas Informant")
    (play-from-hand state :runner "Bank Job")
    (play-from-hand state :runner "Rosetta 2.0")
    (play-from-hand state :runner "Magnum Opus")
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner)
    (play-and-score state "Paper Trail")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 2 (count (:discard (get-runner)))))
    (is (some? (get-resource state 0)))
    (is (= 1 (count (get-resource state))))
    (is (some? (get-program state 0)))
    (is (some? (get-hardware state 0)))))
