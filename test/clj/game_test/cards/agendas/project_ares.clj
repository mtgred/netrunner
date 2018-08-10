(ns game-test.cards.agendas.project-ares
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest project-ares
  ;; Project Ares
  (do-game
    (new-game {:corp {:deck [(qty "Project Ares" 2)]}
               :runner {:deck ["Clone Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (take-credits state :runner)
    (play-and-score state "Project Ares")
    (is (empty? (get-in @state [:runner :prompt])) "No prompt for Runner if scored with 4 advancement tokens")
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Project Ares" "New remote")
    (let [ares (get-content state :remote2 0)]
      (advance state ares 6)
      (is (= 6 (get-counters (refresh ares) :advancement)))
      (core/score state :corp {:card (refresh ares)})
      (is (prompt-is-card? state :runner ares) "Runner has Ares prompt to trash installed cards"))
    (click-card state :runner (find-card "Clone Chip" (:hardware (:rig (get-runner)))))
    (is (empty? (get-in @state [:runner :prompt])) "Runner must trash 2 cards but only has 1 card in rig, prompt ended")
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 1 (:bad-publicity (get-corp))))))
