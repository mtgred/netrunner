(ns game.core.say-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.mark :refer :all]
   [game.test-framework :refer :all]))

(deftest chat-commands

  (testing "/adv-counter"
    (let [user {:username "Corp"}]
      (do-game
        (new-game {:corp {:hand ["Project Beale"]}})
        (play-from-hand state :corp "Project Beale" "New remote")
        (let [pb (get-content state :remote1 0)]
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale starts with 0 counters")
          (core/command-parser state :corp {:user user :text "/adv-counter 3"})
          (click-card state :corp "Project Beale")
          (is (= 3 (get-counters (refresh pb) :advancement)) "Project Beale gained 3 counters")
          (core/command-parser state :corp {:user user :text "/adv-counter -1"})
          (click-card state :corp "Project Beale")
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale gained 0 counters on negative adv")
          (core/command-parser state :corp {:user user :text "/adv-counter 999999999999999999999999999999999"})
          (click-card state :corp "Project Beale")
          (is (= 1000 (get-counters (refresh pb) :advancement)) "Project Beale gained 1000 counters on super large adv")))))

  (testing "/bp"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :corp {:user user :text "/bp 3"})
        (is (= 3 (count-bad-pub state)) "Should gain 3 bad publicity")
        (core/command-parser state :corp {:user user :text "/bp -5"})
        (is (= -5 (count-bad-pub state)) "Should gain -5 bad publicity")
        (core/command-parser state :corp {:user user :text "/bp 99999999999999999999999999999999999999999999"})
        (is (= 1000 (count-bad-pub state)) "Should gain 1000 bad publicity"))))

  (testing "/charge"
    (let [user {:username "Runner"}]
      (do-game
        (new-game {:runner {:hand ["Daily Casts" "Earthrise Hotel"] :credits 10}})
        (take-credits state :corp)
        (play-from-hand state :runner "Daily Casts")
        (play-from-hand state :runner "Earthrise Hotel")
        (let [cast (get-resource state 0)
              hotel (get-resource state 1)]
          (is (= 3 (get-counters hotel :power)))
          (core/command-parser state :runner {:user user :text "/charge"})
          (click-card state :runner cast)
          (is (no-prompt? state :runner) "Prompt is cleared")
          (is (zero? (get-counters (refresh cast) :power)) "Cannot charge Daily Casts")
          (core/command-parser state :runner {:user user :text "/charge"})
          (click-card state :runner hotel)
          (is (no-prompt? state :runner) "Prompt is cleared")
          (is (= 4 (get-counters (refresh hotel) :power)) "Charged Earthrise Hotel")))))

  (testing "/click"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :corp {:user user :text "/click 3"})
        (is (= 3 (:click (get-corp))) "Corp has 3 clicks")
        (core/command-parser state :corp {:user user :text "/click -5"})
        (is (= 0 (:click (get-corp))) "Corp has 0 clicks")
        (core/command-parser state :corp {:user user :text "/click 99999999999999999999999999999999999999999999"})
        (is (= 1000 (:click (get-corp))) "Corp has 1000 clicks"))))

  (testing "/counter"
    (let [user {:username "Corp"}]
      (do-game
        (new-game {:corp {:hand ["Project Beale"]}})
        (play-from-hand state :corp "Project Beale" "New remote")
        (let [pb (get-content state :remote1 0)]
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale starts with 0 counters")
          (core/command-parser state :corp {:user user :text "/counter 3"})
          (click-card state :corp "Project Beale")
          (is (= 3 (get-counters (refresh pb) :advancement)) "Project Beale gained 3 counters")
          (core/command-parser state :corp {:user user :text "/counter -1"})
          (click-card state :corp "Project Beale")
          (is (= 0 (get-counters (refresh pb) :advancement)) "Project Beale gained 0 counters on negative adv")
          (core/command-parser state :corp {:user user :text "/counter 999999999999999999999999999999999"})
          (click-card state :corp "Project Beale")
          (is (= 1000 (get-counters (refresh pb) :advancement)) "Project Beale gained 1000 counters on super large adv")))))

  (testing "/credit"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :corp {:user user :text "/credit 3"})
        (is (= 3 (:credit (get-corp))) "Corp has 3 credits")
        (core/command-parser state :corp {:user user :text "/credit -5"})
        (is (= 0 (:credit (get-corp))) "Corp has 0 credits")
        (core/command-parser state :corp {:user user :text "/credit 99999999999999999999999999999999999999999999"})
        (is (= 1000 (:credit (get-corp))) "Corp has 1000 credits"))))

  (testing "/discard #n"
    (let [user {:username "Runner"}]
      (testing "Add card with long title"
        (do-game
          (new-game {:runner {:hand ["Cache"]}})
          (take-credits state :corp)
          (is (= ["Cache"] (->> (get-runner) :hand (mapv :title))) "Cache should be in hand")
          (core/command-parser state :runner {:user user :text "/discard #1"})
          (is (empty? (:hand (get-runner))) "Runner has empty grip")))))

  (testing "/end-run"
    (let [user {:username "Corp"}]
      (testing "Ends an active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/command-parser state :corp {:user user :text "/end-run"})
          (is (not (:run @state)) "Run has ended")))
      (testing "Does nothing with no active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (core/command-parser state :corp {:user user :text "/end-run"})
          (is (not (:run @state)))
          (is (not (:last-run (:register (get-runner))))
              "Last-run isn't marked as there was no run")))
      (testing "Does nothing when used by Runner"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/command-parser state :runner {:user user :text "/end-run"})
          (is (:run @state) "Run is still active")))))

  (testing "/jack-out"
    (let [user {:username "Runner"}]
      (testing "Ends an active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/command-parser state :runner {:user user :text "/jack-out"})
          (is (not (:run @state)) "Run has ended")))
      (testing "Does nothing with no active run"
        (do-game
          (new-game)
          (take-credits state :corp)
          (core/command-parser state :runner {:user user :text "/jack-out"})
          (is (not (:run @state)))
          (is (not (:last-run (:register (get-runner))))
              "Last-run isn't marked as there was no run")))
      (testing "Does nothing when used by Corp"
        (do-game
          (new-game)
          (take-credits state :corp)
          (run-on state "HQ")
          (core/command-parser state :corp {:user user :text "/jack-out"})
          (is (:run @state) "Run is still active")))))

  (testing "/link"
    (let [user {:username "Runner"}]
      (testing "Can increase link"
        (do-game
          (new-game)
          (is (changed? [(get-link state) 1]
                (core/command-parser state :runner {:user user :text "/link 1"}))
              "Link increases by 1")))

      (testing "/link sizes"
        (do-game
          (new-game)
          (core/command-parser state :runner {:user user :text "/link 3"})
          (is (= 3 (get-link state)) "runner has 3 link")
          (core/command-parser state :runner {:user user :text "/link -5"})
          (is (= 0 (get-link state)) "runner has 0 link")
          (core/command-parser state :runner {:user user :text "/link 99999999999999999999999999999999999999999999"})
          (is (= 1000 (get-link state)) "runner has 1000 link")))))

  (testing "/memory"
      (let [user {:username "Runner"}]
        (do-game
          (new-game)
          (core/command-parser state :runner {:user user :text "/memory 3"})
          (is (= 3 (:used (:memory (get-runner)))) "runner has 3 memory")
          (core/command-parser state :runner {:user user :text "/memory -5"})
          (is (= -5 (:used (:memory (get-runner)))) "runner has -5 memory")
          (core/command-parser state :runner {:user user :text "/memory 99999999999999999999999999999999999999999999"})
          (is (= 1000 (:used (:memory (get-runner)))) "runner has 1000 memory"))))

  (testing "/mark"
      (let [user {:username "Runner"}]
        (do-game
          (new-game)
          (core/command-parser state :runner {:user user :text "/mark"})
          (is (some? (:mark @state)) "Mark identified"))))

  (testing "/roll"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :runner {:user user :text "/roll 6"})
        (is (second-last-log-contains? state "rolls a 6 sided die") "Correct message, reasonable number")
        (core/command-parser state :runner {:user user :text "/roll -5"})
        (is (second-last-log-contains? state "rolls a 1 sided die") "Correct message, negative number")
        (core/command-parser state :runner {:user user :text "/roll 99999999999999999999999999999999999999999999"})
        (is (second-last-log-contains? state "rolls a 1000 sided die") "Correct message, very large number"))))

  (testing "/sabotage"
    (let [user {:username "Runner"}]
      (do-game
        (new-game {:corp {:hand [(qty "Hedge Fund" 5)] :deck [(qty "IPO" 5)]}})
        (core/command-parser state :runner {:user user :text "/sabotage 2"})
        (click-card state :corp (nth (:hand (get-corp)) 0))
        (click-prompt state :corp "Done")
        (is (= 2 (count (:discard (get-corp)))) "Archives has 2 card"))))

  (testing "/set-mark"
    (let [user {:username "Runner"}]
      (testing "Setting the mark to a central server"
        (do-game
          (new-game)
          (core/command-parser state :runner {:user user :text "/set-mark HQ"})
          (is (is-mark? state :hq))))
      (testing "Setting the mark overrides previous mark, if any"
        (do-game
          (new-game)
          (set-mark state :rd)
          (core/command-parser state :runner {:user user :text "/set-mark Archives"})
          (is (is-mark? state :archives))))
      (testing "Setting the mark doesn't work on remote servers"
        (do-game
          (new-game)
          (core/command-parser state :runner {:user user :text "/set-mark Server 1"})
          (is (nil? (:mark @state)))))))

  (testing "/summon"
    (let [user {:username "Runner"}]
      (testing "Add card with short title"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (core/command-parser state :runner {:user user :text "/summon DDoS"})
          (is (= ["DDoS"] (->> (get-runner) :hand (mapv :title))) "DDoS should now be added into hand")))

      (testing "Add non-existant card"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (core/command-parser state :runner {:user user :text "/summon Probably Not A Real Card Name"})
          (is (empty? (:hand (get-runner))) "Runner still has an empty grip")))

      (testing "Add card with long title"
        (do-game
          (new-game {:runner {:hand []}})
          (take-credits state :corp)
          (is (empty? (:hand (get-runner))) "Runner starts with empty grip")
          (core/command-parser state :runner {:user user :text "/summon Harmony AR Therapy"})
          (is (= ["Harmony AR Therapy"] (->> (get-runner) :hand (mapv :title))) "Harmony AR Therapy should now be added into hand")))))

  (testing "/tag"
    (let [user {:username "Runner"}]
      (do-game
        (new-game)
        (core/command-parser state :runner {:user user :text "/tag 3"})
        (is (= 3 (:base (:tag (get-runner)))) "Runner has 3 tags")
        (core/command-parser state :runner {:user user :text "/tag -5"})
        (is (= 0 (:base (:tag (get-runner)))) "Runner has 0 tags")
        (core/command-parser state :runner {:user user :text "/tag 99999999999999999999999999999999999999999999"})
        (is (= 1000 (:base (:tag (get-runner)))) "Runner has 1000 tags"))))

  (testing "/take-core"
    (let [user {:username "Runner"}]
      (do-game
        (new-game)
        (core/command-parser state :runner {:user user :text "/take-core 3"})
        (is (= 3 (:brain-damage (get-runner))) "Runner gains 3 core")
        (core/command-parser state :runner {:user user :text "/take-core -5"})
        (is (= 3 (:brain-damage (get-runner))) "Runner gains 0 core")
        (core/command-parser state :runner {:user user :text "/take-core 99999999999999999999999999999999999999999999"})
        (is (= 1003 (:brain-damage (get-runner))) "Runner gains 1000 core"))))

  (testing "/trace"
    (let [user {:username "Corp"}]
      (do-game
        (new-game)
        (core/command-parser state :corp {:user user :text "/trace 6"})
        (is (= 6 (:base (prompt-map :corp))) "Base trace should now be 6")
        (core/command-parser state :corp {:user user :text "/trace -5"})
        (is (= -5 (:base (prompt-map :corp))) "Base trace should now be -5")
        (core/command-parser state :corp {:user user :text "/trace 99999999999999999999999999999999999999999999"})
        (is (= 1000 (:base (prompt-map :corp))) "Base trace should now be 1000"))))

  (testing "/unique"
    (let [user {:username "Runner"}]
      (do-game
        (new-game {:runner {:hand [(qty "Wireless Net Pavilion" 2)]
                            :credit 100}})
        (take-credits state :corp)
        (play-from-hand state :runner "Wireless Net Pavilion")
        (let [wnp1 (get-resource state 0)]
          (core/command-parser state :runner {:user user :text "/unique"})
          (is (last-log-contains? state "\\[!\\]Runner uses a command: /unique") "Correct message")
          (click-card state :runner wnp1)
          (is (not (unique? (refresh wnp1))) "WNP is not unique anymore")
          (is (last-log-contains? state "Runner uses /unique command to make Wireless Net Pavilion not unique\\.") "Correct message")
          (play-from-hand state :runner "Wireless Net Pavilion")
          (is (zero? (count (:hand (get-runner)))) "Both cards have been installed")
          (is (= 2 (count (get-resource state))))
          (is (every? #(= "Wireless Net Pavilion" (:title %)) (get-resource state))))))))
