(ns game-test.cards.upgrades.arella-salvatore
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest arella-salvatore
  ;; Arella Salvatore - when an agenda is scored from this server, install a card from hq w/ advancement token
  (testing "Install to server"
    (do-game
      (new-game {:corp {:deck ["Arella Salvatore" "Bryan Stinson" (qty "TGTBT" 2)]}})
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "TGTBT" "Server 1")
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [arella (get-content state :remote1 0)
            same-tg (get-content state :remote1 1)
            diff-tg (get-content state :remote2 0)]
        (core/rez state :corp arella)
        (score-agenda state :corp (refresh diff-tg))
        (is (empty? (get-in @state [:corp :prompt])) "Arella not triggered for different remote score")
        (is (= 1 (count (get-scored state :corp))) "1 Agenda scored")
        (score-agenda state :corp (refresh same-tg))
        (click-card state :corp (find-card "Bryan Stinson" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (is (= 2 (count (get-scored state :corp))) "2 Agendas scored")
        (is (= 1 (count (get-content state :remote3))) "Bryan installed in new remote")
        (is (= 1 (get-counters (get-content state :remote3 0) :advancement)) "Bryan has 1 advancement counter"))))
  (testing "Interaction w/ other on-scored triggers"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Arella Salvatore" "Domestic Sleepers" "Project Vitruvius" "Hedge Fund"]}})
      (starting-hand state :corp ["Arella Salvatore" "Domestic Sleepers"])
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "Domestic Sleepers" "Server 1")
      (let [arella (get-content state :remote1 0)
            domest (get-content state :remote1 1)]
        (core/rez state :corp arella)
        (score-agenda state :corp (refresh domest))
        ;; Simultaneous prompt: Sportsmetal automatically triggers, as Arella is silent because there are no installable cards in HQ
        (click-prompt state :corp "2 cards")
        ;; Arella is no longer silent and now triggers
        (click-card state :corp (find-card "Project Vitruvius" (:hand (get-corp))))
        (click-prompt state :corp "Server 1")
        (is (= 2 (count (get-content state :remote1))) "Agenda installed in server 1")
        (is (= 1 (get-counters (get-content state :remote1 1) :advancement)) "Agenda has 1 advancement counter"))))
  (testing "No cost"
    (do-game
      (new-game {:corp {:deck ["Arella Salvatore" "TGTBT" (qty "Ice Wall" 2)]}})
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "Arella Salvatore" "New remote")
      (play-from-hand state :corp "TGTBT" "Server 1")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 1 (count (get-ice state :hq))) "One ice on hq")
      (let [arella (get-content state :remote1 0)
            tg (get-content state :remote1 1)]
        (core/rez state :corp arella)
        (score-agenda state :corp (refresh tg))
        (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 2 (count (get-ice state :hq))) "Two ice on hq")
        (is (= 1 (get-counters (get-ice state :hq 1) :advancement)) "Ice Wall has 1 counter")))))
