(ns game.core.commands-test
  (:require [game.core :as core :refer [parse-and-perform-command]]
            [game.core.card :refer :all]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

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

(deftest chat-commands
  (testing "/adv-counter"
    (let [user {:username "Corp"}]
      (do-game
        (new-game {:corp {:hand ["Project Beale"]}})
        (play-from-hand state :corp "Project Beale" "New remote")
        (let [pb (get-content state :remote1 0)]
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale starts with 0 counters")
          (parse-and-perform-command state :corp user "/adv-counter 3")
          (click-card state :corp "Project Beale")
          (is (= 3 (get-counters (refresh pb) :advancement)) "Project Beale gained 3 counters")
          (parse-and-perform-command state :corp user "/adv-counter -1")
          (click-card state :corp "Project Beale")
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale gained 0 counters on negative adv")
          (parse-and-perform-command state :corp user "/adv-counter 999999999999999999999999999999999")
          (click-card state :corp "Project Beale")
          (is (= 1000 (get-counters (refresh pb) :advancement)) "Project Beale gained 1000 counters on super large adv")))))
  (testing "/bp"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (parse-and-perform-command state :corp user "/bp 3")
        (is (= 3 (count-bad-pub state)) "Should gain 3 bad publicity")
        (parse-and-perform-command state :corp user "/bp -5")
        (is (= -5 (count-bad-pub state)) "Should gain -5 bad publicity")
        (parse-and-perform-command state :corp user "/bp 99999999999999999999999999999999999999999999")
        (is (= 1000 (count-bad-pub state)) "Should gain 1000 bad publicity"))))
  (testing "/click"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (parse-and-perform-command state :corp user "/click 3")
        (is (= 3 (:click (get-corp))) "Corp has 3 clicks")
        (parse-and-perform-command state :corp user "/click -5")
        (is (= 0 (:click (get-corp))) "Corp has 0 clicks")
        (parse-and-perform-command state :corp user "/click 99999999999999999999999999999999999999999999")
        (is (= 1000 (:click (get-corp))) "Corp has 1000 clicks"))))
  (testing "/counter"
    (let [user {:username "Corp"}]
      (do-game
        (new-game {:corp {:hand ["Project Beale"]}})
        (play-from-hand state :corp "Project Beale" "New remote")
        (let [pb (get-content state :remote1 0)]
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale starts with 0 counters")
          (parse-and-perform-command state :corp user "/counter 3")
          (click-card state :corp "Project Beale")
          (is (= 3 (get-counters (refresh pb) :advancement)) "Project Beale gained 3 counters")
          (parse-and-perform-command state :corp user "/counter -1")
          (click-card state :corp "Project Beale")
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale gained 0 counters on negative adv")
          (parse-and-perform-command state :corp user "/counter 999999999999999999999999999999999")
          (click-card state :corp "Project Beale")
          (is (= 1000 (get-counters (refresh pb) :advancement)) "Project Beale gained 1000 counters on super large adv")))))
  (testing "/credit"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (parse-and-perform-command state :corp user "/credit 3")
        (is (= 3 (:credit (get-corp))) "Corp has 3 credits")
        (parse-and-perform-command state :corp user "/credit -5")
        (is (= 0 (:credit (get-corp))) "Corp has 0 credits")
        (parse-and-perform-command state :corp user "/credit 99999999999999999999999999999999999999999999")
        (is (= 1000 (:credit (get-corp))) "Corp has 1000 credits"))))
  (testing "/discard #n"
    (let [user {:username "Runner"}]
      (testing "Add card with long title"
        (do-game
          (new-game {:runner {:hand ["Cache"]}})
          (take-credits state :corp)
          (is (= ["Cache"] (->> (get-runner) :hand (mapv :title))) "Cache should be in hand")
          (parse-and-perform-command state :runner user "/discard #1")
          (is (empty? (:hand (get-runner))) "Runner has empty grip")))))
  (testing "/end-run"
    (let [user {:username "Corp"}]
      (testing "Ends an active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (parse-and-perform-command state :corp user "/end-run")
          (is (not (:run @state)) "Run has ended")))
      (testing "Does nothing with no active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (parse-and-perform-command state :corp user "/end-run")
          (is (not (:run @state)))
          (is (not (:last-run (:register (get-runner))))
              "Last-run isn't marked as there was no run")))
      (testing "Does nothing when used by Runner"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (parse-and-perform-command state :runner user "/end-run")
          (is (:run @state) "Run is still active")))))
  (testing "/jack-out"
    (let [user {:username "Runner"}]
      (testing "Ends an active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (parse-and-perform-command state :runner user "/jack-out")
          (is (not (:run @state)) "Run has ended")))
      (testing "Does nothing with no active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (parse-and-perform-command state :runner user "/jack-out")
          (is (not (:run @state)))
          (is (not (:last-run (:register (get-runner))))
              "Last-run isn't marked as there was no run")))
      (testing "Does nothing when used by Corp"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (parse-and-perform-command state :corp user "/jack-out")
          (is (:run @state) "Run is still active")))))
  (testing "/link"
    (let [user {:username "Runner"}]
      (testing "Can increase link"
        (do-game
          (new-game)
          (changes-val-macro
            1 (get-link state)
            "Link increases by 1"
            (parse-and-perform-command state :runner user "/link 1"))))
      (testing "/link sizes"
        (do-game
          (new-game)
          (parse-and-perform-command state :runner user "/link 3")
          (is (= 3 (get-link state)) "runner has 3 link")
          (parse-and-perform-command state :runner user "/link -5")
          (is (= 0 (get-link state)) "runner has 0 link")
          (parse-and-perform-command state :runner user "/link 99999999999999999999999999999999999999999999")
          (is (= 1000 (get-link state)) "runner has 1000 link")))))
  (testing "/install"
    (testing "corp"
      (let [user {:username "Corp"}]
        (do-game
          (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                            :hand ["Honeyfarm"]}})
          (parse-and-perform-command state :corp user "/install")
          (is (prompt-is-type? state :runner :waiting))
          (is (prompt-is-type? state :corp :select))
          (is (= "Select a card in hand to install" (:msg (prompt-map :corp))))
          (is (nil? (:resolving-action @state)))
          (click-card state :corp "Honeyfarm")
          (is (= ["New remote"] (prompt-buttons :corp)))
          (click-prompt state :corp "New remote")
          (is (empty? (:hand (get-corp))))
          (is (find-card "Honeyfarm" (get-content state :remote1)))
          (is (empty? (:prompt (get-corp))))
          (is (empty? (:prompt (get-runner)))))))
    (testing "runner"
      (let [user {:username "Runner"}]
        (do-game
          (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                              :hand ["Corroder"]}})
          (parse-and-perform-command state :runner user "/install")
          (is (prompt-is-type? state :corp :waiting))
          (is (prompt-is-type? state :runner :select))
          (is (= "Select a card in hand to install" (:msg (prompt-map :runner))))
          (is (nil? (:resolving-action @state)))
          (click-card state :runner "Corroder")
          (is (empty? (:hand (get-runner))))
          (is (find-card "Corroder" (get-program state)))
          (is (empty? (:prompt (get-corp))))
          (is (empty? (:prompt (get-runner))))))))
  (testing "/memory"
      (let [user {:username "Runner"}]
        (do-game
          (new-game)
          (parse-and-perform-command state :runner user "/memory 3")
          (is (= 3 (:used (:memory (get-runner)))) "runner has 3 memory")
          (parse-and-perform-command state :runner user "/memory -5")
          (is (= -5 (:used (:memory (get-runner)))) "runner has -5 memory")
          (parse-and-perform-command state :runner user "/memory 99999999999999999999999999999999999999999999")
          (is (= 1000 (:used (:memory (get-runner)))) "runner has 1000 memory"))))
  (testing "/roll"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (parse-and-perform-command state :runner user "/roll 6")
        (is (second-last-log-contains? state "rolls a 6 sided die") "Correct message, reasonable number")
        (parse-and-perform-command state :runner user "/roll -5")
        (is (second-last-log-contains? state "rolls a 1 sided die") "Correct message, negative number")
        (parse-and-perform-command state :runner user "/roll 99999999999999999999999999999999999999999999")
        (is (second-last-log-contains? state "rolls a 1000 sided die") "Correct message, very large number"))))
  (testing "/summon"
    (let [user {:username "Runner"}]
      (testing "Add card with short title"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (parse-and-perform-command state :runner user "/summon DDoS")
          (is (= ["DDoS"] (->> (get-runner) :hand (mapv :title))) "DDoS should now be added into hand")))
      (testing "Add non-existant card"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (parse-and-perform-command state :runner user "/summon Probably Not A Real Card Name")
          (is (empty? (:hand (get-runner))) "Runner still has an empty grip")))
      (testing "Add card with long title"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (parse-and-perform-command state :runner user "/summon Harmony AR Therapy")
          (is (= ["Harmony AR Therapy"] (->> (get-runner) :hand (mapv :title))) "Harmony AR Therapy should now be added into hand")))))
  (testing "/tag"
    (let [user {:username "Runner"}]
      (do-game
        (new-game)
        (parse-and-perform-command state :runner user "/tag 3")
        (is (= 3 (:base (:tag (get-runner)))) "Runner has 3 tags")
        (parse-and-perform-command state :runner user "/tag -5")
        (is (= 0 (:base (:tag (get-runner)))) "Runner has 0 tags")
        (parse-and-perform-command state :runner user "/tag 99999999999999999999999999999999999999999999")
        (is (= 1000 (:base (:tag (get-runner)))) "Runner has 1000 tags"))))
  (testing "/take-brain"
    (let [user {:username "Runner"}]
      (do-game
        (new-game)
        (parse-and-perform-command state :runner user "/take-brain 3")
        (is (= 3 (:brain-damage (get-runner))) "Runner gains 3 brain")
        (parse-and-perform-command state :runner user "/take-brain -5")
        (is (= 3 (:brain-damage (get-runner))) "Runner gains 0 brain")
        (parse-and-perform-command state :runner user "/take-brain 99999999999999999999999999999999999999999999")
        (is (= 1003 (:brain-damage (get-runner))) "Runner gains 1000 brain"))))
  (testing "/trace"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (parse-and-perform-command state :corp user "/trace 6")
        (is (= 6 (:base (prompt-map :corp))) "Base trace should now be 6")
        (parse-and-perform-command state :corp user "/trace -5")
        (is (= -5 (:base (prompt-map :corp))) "Base trace should now be -5")
        (parse-and-perform-command state :corp user "/trace 99999999999999999999999999999999999999999999")
        (is (= 1000 (:base (prompt-map :corp))) "Base trace should now be 1000"))))
  (testing "/unique"
    (let [user {:username "Runner"}]
      (testing "Works with Wireless Net Pavillion"
        (do-game
          (new-game {:runner {:hand [(qty "Wireless Net Pavilion" 2)]}})
          (take-credits state :corp)
          (let [wnp1 (nth (:hand (get-runner)) 0)
                wnp2 (nth (:hand (get-runner)) 1)]
            (core/play state :runner {:card wnp1})
            (core/play state :runner {:card wnp2})
            (is (= 1 (count (:hand (get-runner)))) "Second WNP was not installed")
            (is (:uniqueness wnp2) "WNP is unique")
            (parse-and-perform-command state :runner user "/unique")
            (is (last-log-contains? state "\\[!\\]Runner uses a command: /unique") "Correct message")
            (click-card state :runner wnp2)
            (is (not (:uniqueness (refresh wnp2))) "WNP is not unique anymore")
            (is (last-log-contains? state "Runner uses /unique command to make Wireless Net Pavilion not unique\\.") "Correct message")
            (core/play state :runner {:card wnp2})
            (is (zero? (count (:hand (get-runner)))) "Both cards have been installed")))))))
