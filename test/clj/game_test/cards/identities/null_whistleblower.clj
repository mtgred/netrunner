(ns game-test.cards.identities.null-whistleblower
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest null-whistleblower
  ;; Null
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Wraparound" 3)]}
                 :runner {:id "Null: Whistleblower"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [null (get-in @state [:runner :identity])
            wrap1 (get-ice state :hq 0)
            wrap2 (get-ice state :hq 1)]
        (card-ability state :runner null 0)
        (is (empty? (:prompt (get-runner))) "Ability won't work on unrezzed ICE")
        (core/rez state :corp wrap2)
        (card-ability state :runner null 0)
        (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
        (run-continue state)
        (core/rez state :corp wrap1)
        (card-ability state :runner null 0)
        (is (empty? (:prompt (get-runner))) "Ability already used this turn")
        (run-jack-out state)
        (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))
  (testing "does not affect next ice when current is trashed. Issue #1788."
    (do-game
      (new-game {:corp {:deck ["Wraparound" "Spiderweb"]}
                 :runner {:id "Null: Whistleblower"
                          :deck [(qty "Parasite" 3)]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (let [null (get-in @state [:runner :identity])
            spider (get-ice state :hq 0)
            wrap (get-ice state :hq 1)]
        (core/rez state :corp spider)
        (core/rez state :corp wrap)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner (refresh spider))
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner null 0)
        (click-card state :runner (first (:hand (get-runner))))
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Null")
        (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null")))))
