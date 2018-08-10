(ns game-test.cards.agendas.research-grant
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest research-grant
  ;; Research Grant
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Research Grant" 2)]}})
      (play-from-hand state :corp "Research Grant" "New remote")
      (play-and-score state "Research Grant")
      (click-card state :corp (get-content state :remote1 0))
      (is (= 2 (count (:scored (get-corp)))) "2 copies of Research Grant scored")))
  (testing "vs Leela"
    ;; Issue #3069
    (do-game
      (new-game {:corp {:deck [(qty "Research Grant" 2) (qty "Ice Wall" 2)]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck ["Sure Gamble"]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Research Grant" "New remote")
      (play-and-score state "Research Grant")
      (click-card state :corp (get-content state :remote1 0))
      (is (= 2 (count (:scored (get-corp)))) "2 copies of Research Grant scored")
      (click-card state :runner (get-ice state :hq 0))
      (click-card state :runner (get-ice state :rd 0))
      (is (empty? (:effect-completed @state)) "All score and Leela effects resolved"))))
