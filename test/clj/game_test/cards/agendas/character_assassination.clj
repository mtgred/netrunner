(ns game-test.cards.agendas.character-assassination
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest character-assassination
  ;; Character Assassination
  (do-game
    (new-game {:corp {:deck ["Character Assassination"]}
               :runner {:deck ["Fall Guy" "Kati Jones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (play-from-hand state :runner "Fall Guy")
    (take-credits state :runner)
    (play-and-score state "Character Assassination")
    (let [kati (get-resource state 0)]
      (click-card state :corp kati)
      (is (empty? (:prompt (get-runner))) "Fall Guy prevention didn't occur")
      (is (= 1 (count (:discard (get-runner)))) "Kati Jones trashed"))))
