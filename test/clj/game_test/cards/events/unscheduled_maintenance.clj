(ns game-test.cards.events.unscheduled-maintenance
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest unscheduled-maintenance
  ;; Unscheduled Maintenance - prevent Corp from installing more than 1 ICE per turn
  (do-game
    (new-game {:corp {:deck [(qty "Vanilla" 2) "Breaking News"]}
               :runner {:deck ["Unscheduled Maintenance"]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Unscheduled Maintenance")
    (take-credits state :runner)
    (play-from-hand state :corp "Vanilla" "HQ")
    (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "First ICE install of turn allowed")
    (play-from-hand state :corp "Vanilla" "R&D")
    (is (empty? (get-in @state [:corp :servers :rd :ices])) "Second ICE install of turn blocked")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Vanilla" "R&D")
    (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Current trashed; second ICE install of turn allowed")))
