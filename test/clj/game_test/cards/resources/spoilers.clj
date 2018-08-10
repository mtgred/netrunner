(ns game-test.cards.resources.spoilers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest spoilers
  ;; Spoilers - Mill the Corp when it scores an agenda
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Hedge Fund"]}
               :runner {:deck ["Spoilers"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Spoilers")
    (take-credits state :runner)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (is (= 1 (count (:deck (get-corp)))))
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :corp ht)
      (is (= 1 (count (:discard (get-corp)))))
      (is (zero? (count (:deck (get-corp)))) "Last card from R&D milled"))))
