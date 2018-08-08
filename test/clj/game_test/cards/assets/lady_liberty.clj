(ns game-test.cards.assets.lady-liberty
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest lady-liberty
  ;; Lady Liberty - Score agenda from hand equal to number of power counters on Lady Libery
  (testing "Basic behavior"
    (do-game
      (new-game {:corp {:deck ["Lady Liberty" "Breaking News" "Ikawah Project"]}})
      (play-from-hand state :corp "Lady Liberty" "New remote")
      (let [ll (get-content state :remote1 0)]
        (core/rez state :corp ll)
        (take-credits state :corp)
        (is (zero? (get-counters (refresh ll) :power)) "Lady Liberty starts with 0 counters")
        (take-credits state :runner)
        (is (= 1 (get-counters (refresh ll) :power)) "Lady Liberty gains a power counter on start of turn")
        (is (= 2 (count (:hand (get-corp)))) "Two cards in hand")
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Breaking News" (:hand (get-corp))))
        (is (= 1 (count (:hand (get-corp)))) "One card in hand")
        (is (= 1 (count (:scored (get-corp)))) "One card in score area")
        (is (= 1 (:agenda-point (get-corp))) "Gained agenda point")
        (take-credits state :corp)
        (take-credits state :runner)
        (card-ability state :corp (refresh ll) 0)
        (is (empty? (:prompt (get-corp))) "No prompt if no matching agenda")
        (take-credits state :corp)
        (take-credits state :runner)
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Ikawah Project" (:hand (get-corp))))
        (is (empty? (:hand (get-corp))) "No cards in hand")
        (is (= 2 (count (:scored (get-corp)))) "Two cards in score area")
        (is (= 4 (:agenda-point (get-corp))) "Gained 3 agenda points"))))
  (testing "Agenda constant effects"
    (do-game
      (new-game {:corp {:deck ["Lady Liberty" "Self-Destruct Chips"]}})
      (play-from-hand state :corp "Lady Liberty" "New remote")
      (let [ll (get-content state :remote1 0)]
        (core/rez state :corp ll)
        (take-credits state :corp)
        (take-credits state :runner)
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Self-Destruct Chips" (:hand (get-corp))))
        (is (= 1 (:agenda-point (get-corp))) "Gained 1 agenda points")
        (is (= 4 (core/hand-size state :runner)) "Runner hand size reduced by 1"))))
  (testing "Agenda events"
    (do-game
      (new-game {:corp {:deck ["Lady Liberty" "Puppet Master"]}})
      (play-from-hand state :corp "Lady Liberty" "New remote")
      (let [ll (get-content state :remote1 0)]
        (core/rez state :corp ll)
        (dotimes [i 3]
          (take-credits state :corp)
          (take-credits state :runner))
        (card-ability state :corp (refresh ll) 0)
        (click-card state :corp (find-card "Puppet Master" (:hand (get-corp))))
        (is (= 3 (:agenda-point (get-corp))) "Gained 3 agenda points")
        (take-credits state :corp)
        (run-on state "HQ")
        (run-successful state)
        (is (= "Select a card to place 1 advancement token on" (:msg (first (:prompt (get-corp))))) "Puppet Master event fired")))))
