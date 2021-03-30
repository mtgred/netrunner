(ns game.core.actions-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest undo-turn-test
  (do-game
    (new-game)
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 1 (:click (get-corp))) "Corp spent 2 clicks")
    (is (= 13 (:credit (get-corp))) "Corp has 13 credits")
    (is (= 1 (count (:hand (get-corp)))) "Corp has 1 card in HQ")
    (core/command-undo-turn state :runner)
    (core/command-undo-turn state :corp)
    (is (= 3 (count (:hand (get-corp)))) "Corp has 3 cards in HQ")
    (is (zero? (:click (get-corp))) "Corp has no clicks - turn not yet started")
    (is (= 5 (:credit (get-corp))) "Corp has 5 credits")))

(deftest undo-click-test
  (do-game
    (new-game {:corp {:deck ["Ikawah Project"]}
               :runner {:deck ["Day Job"]}})
    (play-from-hand state :corp "Ikawah Project" "New remote")
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay to steal")
    (is (= 2 (:click (get-runner))) "Runner should lose 1 click to steal")
    (is (= 3 (:credit (get-runner))) "Runner should lose 2 credits to steal")
    (is (= 1 (count (:scored (get-runner)))) "Runner should steal Ikawah Project")
    (core/command-undo-click state :corp)
    (is (= 1 (count (:scored (get-runner)))) "Corp attempt to undo click does nothing")
    (core/command-undo-click state :runner)
    (is (zero? (count (:scored (get-runner)))) "Runner attempt to undo click works ok")
    (is (= 4 (:click (get-runner))) "Runner back to 4 clicks")
    (is (= 5 (:credit (get-runner))) "Runner back to 5 credits")
    (play-from-hand state :runner "Day Job")
    (is (zero? (:click (get-runner))) "Runner spent 4 clicks")
    (core/command-undo-click state :runner)
    (is (= 4 (:click (get-runner))) "Runner back to 4 clicks")
    (is (= 5 (:credit (get-runner))) "Runner back to 5 credits")))

(deftest undo-click-with-bioroid-cost-test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "R&D")
    (take-credits state :corp)
    (run-on state :rd)
    (let [ice (get-ice state :rd 0)]
      (rez state :corp ice)
      (run-continue state)
      (card-side-ability state :runner ice 0)
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state "Runner loses \\[Click\\] to use Eli 1.0 to break 1 subroutine on Eli 1.0"))
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state "Runner loses \\[Click\\] to use Eli 1.0 to break 1 subroutine on Eli 1.0")))
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "No action")
    (is (not (get-run)))
    (is (= 1 (:click (get-runner))))
    (core/command-undo-click state :runner)
    (is (= 4 (:click (get-runner))))
    (is (last-log-contains? state "Runner uses the undo-click command"))))

(deftest counter-manipulation-commands-test
  ;; Test interactions of various cards with /counter and /adv-counter commands
  (do-game
    (new-game {:corp {:deck ["Adonis Campaign"
                             (qty "Public Support" 2)
                             "Oaktown Renovation"]}})
    ;; Turn 1 Corp, install oaktown and assets
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (play-from-hand state :corp "Public Support" "New remote")
    (play-from-hand state :corp "Public Support" "New remote")
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [adonis (get-content state :remote1 0)
          publics1 (get-content state :remote2 0)
          publics2 (get-content state :remote3 0)
          oaktown (get-content state :remote4 0)]
      (core/advance state :corp {:card (refresh oaktown)})
      (core/advance state :corp {:card (refresh oaktown)})
      (core/advance state :corp {:card (refresh oaktown)})
      (is (= 8 (:credit (get-corp))) "Corp 5+3 creds from Oaktown")
      (core/end-turn state :corp nil)
      (testing "Turn 1 Runner"
        (core/start-turn state :runner nil)
        (take-credits state :runner 3)
        (click-credit state :runner)
        (core/end-turn state :runner nil)
        (rez state :corp (refresh adonis))
        (rez state :corp (refresh publics1)))
      (testing "Turn 2 Corp"
        (core/start-turn state :corp nil)
        (rez state :corp (refresh publics2))
        (is (= 3 (:click (get-corp))))
        (is (= 3 (:credit (get-corp))) "only Adonis money")
        (is (= 9 (get-counters (refresh adonis) :credit)))
        (is (= 2 (get-counters (refresh publics1) :power)))
        (is (= 3 (get-counters (refresh publics2) :power))))
      ;; oops, forgot to rez 2nd public support before start of turn,
      ;; let me fix it with a /command
      (testing "Advancement and Scoring checks"
        (core/command-counter state :corp ["power" 2])
        (click-card state :corp (refresh publics2))
        (is (= 2 (get-counters (refresh publics2) :power)))
        ;; Oaktown checks and manipulation
        (is (= 3 (get-counters (refresh oaktown) :advancement)))
        (core/command-adv-counter state :corp 2)
        (click-card state :corp (refresh oaktown))
        ;; score should fail, shouldn't be able to score with 2 advancement tokens
        (score state :corp (refresh oaktown))
        (is (zero? (:agenda-point (get-corp))))
        (core/command-adv-counter state :corp 4)
        (click-card state :corp (refresh oaktown))
        (is (= 4 (get-counters (refresh oaktown) :advancement)))
        (is (= 3 (:credit (get-corp))))
        (is (= 3 (:click (get-corp))))
        (score state :corp (refresh oaktown)) ; now the score should go through
        (is (= 2 (:agenda-point (get-corp))))
        (take-credits state :corp))
      (testing "Modifying publics1 and adonis for brevity"
        (is (= 2 (get-counters (refresh publics1) :power)))
        (core/command-counter state :corp ["power" 1])
        (click-card state :corp (refresh publics1))
        (is (= 1 (get-counters (refresh publics1) :power)))
        ;; let's adjust Adonis while at it
        (is (= 9 (get-counters (refresh adonis) :credit)))
        (core/command-counter state :corp ["credit" 3])
        (click-card state :corp (refresh adonis))
        (is (= 3 (get-counters (refresh adonis) :credit))))
      (testing "Turn 2 Runner"
        (take-credits state :runner))
      (testing "Turn 3 Corp"
        (is (= 3 (:agenda-point (get-corp)))) ; cheated PS1 should get scored
        (is (= 9 (:credit (get-corp))))
        ; (is (= :scored (:zone (refresh publics1))))
        (is (= [:servers :remote3 :content] (:zone (refresh publics2))))
        ; (is (= :discard (:zone (refresh adonis))))
        (take-credits state :corp))
      (testing "Turn 3 Runner"
        (take-credits state :runner))
      (testing "Turn 4 Corp"
        (is (= 4 (:agenda-point (get-corp)))) ; PS2 should get scored
        (is (= 12 (:credit (get-corp))))))))

(deftest counter-manipulation-commands-smart-test
  ;; Test interactions of smart counter advancement command
  (do-game
    (new-game {:corp {:deck ["House of Knives"]}})
    (play-from-hand state :corp "House of Knives" "New remote")
    (let [hok (get-content state :remote1 0)]
      (core/command-counter state :corp [3])
      (click-card state :corp (refresh hok))
      (is (= 3 (get-counters (refresh hok) :advancement)))
      (score state :corp (refresh hok)))
    (let [hok-scored (get-scored state :corp 0)]
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should start with 3 counters")
      (core/command-counter state :corp ["virus" 2])
      (click-card state :corp (refresh hok-scored))
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should stay at 3 counters")
      (is (= 2 (get-counters (refresh hok-scored) :virus)) "House of Knives should have 2 virus counters")
      (core/command-counter state :corp [4])
      (click-card state :corp (refresh hok-scored)) ;; doesn't crash with unknown counter type
      (is (empty? (:prompt (get-corp))) "Counter prompt closed")
      (is (= 4 (get-counters (refresh hok-scored) :agenda)) "House of Knives should have 4 agenda counters")
      (is (= 2 (get-counters (refresh hok-scored) :virus)) "House of Knives should have 2 virus counters"))))

(deftest no-action-during-action-test
  (do-game
    (new-game {:runner {:deck [(qty "Sure Gamble" 10)]}})
    (take-credits state :corp)
    (run-on state :hq)
    (changes-val-macro
      0 (count (:hand (get-runner)))
      "Runner draws no cards"
      (core/process-action "draw" state :runner nil))))
