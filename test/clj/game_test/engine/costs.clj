(ns game-test.engine.costs
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest merge-costs
  (testing "Non-damage costs"
    (testing "No defaults, already merged"
      (is (= [[:credit 1]] (core/merge-costs [[:credit 1]]))))
    (testing "Costs are already flattened"
      (is (= [[:credit 1] [:click 1]] (core/merge-costs [[:credit 1 :click 1]]))))
    (testing "Passed as a flattened vec"
      (is (= [[:credit 1]] (core/merge-costs [:credit 1]))))
    (testing "Default type is only element"
      (is (= [[:credit 1]] (core/merge-costs [[:credit]]))))
    (testing "Default plus explicit"
      (is (= [[:click 1] [:credit 1]] (core/merge-costs [[:click :credit 1]]))))
    (testing "Costs ending with defaults expand"
      (is (= [[:credit 1] [:click 1]] (core/merge-costs [[:credit 1 :click]]))))
    (testing "Non-damage costs aren't reordered"
      (is (not= [[:credit 1] [:click 1]] (core/merge-costs [[:click 1 :credit 1]]))))
    (testing "Costs with all defaults are expanded"
      (is (= [[:click 1] [:credit 1]] (core/merge-costs [[:click :credit]]))))
    (testing "Non-damage costs are combined"
      (is (= [[:click 4] [:credit 2]]
             (core/merge-costs [[:click 1] [:click 3] [:credit 1] [:credit 1]]))))
    (testing "Deeply nested costs are flattened"
      (is (= [[:click 3]] (core/merge-costs [[[[[:click 1]]] [[[[[:click 1]]]]]] :click 1])))))
  (testing "Damage costs"
    (testing "Damage costs are moved to the end"
      (is (= [[:credit 1] [:net 1]] (core/merge-costs [[:net 1 :credit 1]]))))
    (testing "Damage isn't combined"
      (is (= [[:net 1] [:net 1]] (core/merge-costs [[:net 1 :net 1]]))))
    (testing "Net, meat, and brain damage are recognized"
      (is (= [[:net 1] [:meat 1] [:brain 1]]
             (core/merge-costs [[:net 1] [:meat 1] [:brain 1]]))))))

(deftest pay-credits
  (testing "Testing several cost messages"
    (do-game
      (new-game {:runner {:hand ["Diesel" "Daily Casts" "Clot" "Career Fair" "Daily Casts" "Sure Gamble" "Misdirection"]}
                 :corp {:hand [(qty "Ice Wall" 2) "Turtlebacks" "Beanstalk Royalties" "Hedge Fund" "Project Beale" "Ben Musashi"]}})
      (core/gain state :corp :click 10)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (last-log-contains? state "Corp spends \\[Click\\] and pays 0 \\[Credits\\] to install ICE protecting HQ.") "Install ICE, zero cost")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (last-log-contains? state "Corp spends \\[Click\\] and pays 1 \\[Credits\\] to install ICE protecting HQ.") "Install ICE, one cost")
      (play-from-hand state :corp "Turtlebacks" "New remote")
      (is (last-log-contains? state "Corp spends \\[Click\\] to install a card in Server 1.") "Install asset, zero cost")
      (play-from-hand state :corp "Ben Musashi" "Server 1")
      (is (last-log-contains? state "Corp spends \\[Click\\] to install a card in Server 1.") "Install upgrade, zero cost")
      (play-from-hand state :corp "Project Beale" "New remote")
      (is (last-log-contains? state "Corp spends \\[Click\\] to install a card in Server 2.") "Install agenda, zero cost")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (second-last-log-contains? state "Corp spends \\[Click\\] and pays 0 \\[Credits\\] to play Beanstalk Royalties.") "Play operation, zero cost")
      (play-from-hand state :corp "Hedge Fund")
      (is (second-last-log-contains? state "Corp spends \\[Click\\] and pays 5 \\[Credits\\] to play Hedge Fund.") "Play operation, five cost")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Diesel")
      (is (second-last-log-contains? state "Runner spends \\[Click\\] and pays 0 \\[Credits\\] to play Diesel.") "Play event, zero cost")
      (play-from-hand state :runner "Sure Gamble")
      (is (second-last-log-contains? state "Runner spends \\[Click\\] and pays 5 \\[Credits\\] to play Sure Gamble.") "Play event, five cost")
      (play-from-hand state :runner "Clot")
      (is (last-log-contains? state "Runner spends \\[Click\\] and pays 2 \\[Credits\\] to install Clot.") "Install program, two cost")
      (play-from-hand state :runner "Misdirection")
      (is (last-log-contains? state "Runner spends \\[Click\\] and pays 0 \\[Credits\\] to install Misdirection.") "Install program, zero cost")
      (play-from-hand state :runner "Career Fair")
      (is (last-log-contains? state "Runner spends \\[Click\\] and pays 0 \\[Credits\\] to play Career Fair.") "Play Career Fair, zero cost")
      (click-card state :runner (find-card "Daily Casts" (:hand (get-runner))))
      (is (last-log-contains? state "Runner pays 0 \\[Credits\\] to install Daily Casts.") "Choose Daily cast, zero cost install")
      (play-from-hand state :runner "Daily Casts")
      (is (last-log-contains? state "Runner spends \\[Click\\] and pays 3 \\[Credits\\] to install Daily Casts.") "Install resource, three cost")
      (run-on state :archives)
      (is (last-log-contains? state "Runner spends \\[Click\\] to make a run on Archives.") "Initiate run, zero cost")))
  (testing "Issue #4295: Auto-pumping Icebreaker with pay-credits prompt"
    (do-game
      (new-game {:runner {:hand ["Corroder" "Net Mercur" "Cloak"]}
                 :corp {:hand ["Fire Wall"]}})
      (play-from-hand state :corp "Fire Wall" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Net Mercur")
      (run-on state :hq)
      (let [cre (:credit (get-runner))
            cor (get-program state 0)
            clo (get-program state 1)
            nm (get-resource state 0)]
        (is (= 2 (:current-strength (refresh cor))) "Corroder starts at 2 strength")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump" :card (refresh cor)})
        (click-card state :runner clo)
        (click-prompt state :runner "Place 1 [Credits]")
        (is (= 5 (:current-strength (refresh cor))) "Corroder is at 5 strength")
        (is (= (- cre 2) (:credit (get-runner))) "Spent 2 (+1 from Cloak) to pump")))))

(deftest pump-and-break
  (testing "Basic test"
    (do-game
      (new-game {:runner {:hand ["Corroder"]}
                 :corp {:hand ["Hive"]}})
      (play-from-hand state :corp "Hive" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (let [cor (get-program state 0)
            hive (get-ice state :hq 0)]
        (is (= 2 (:current-strength (refresh cor))) "Corroder starts at 2 strength")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh cor)})
        (is (= 3 (:current-strength (refresh cor))) "Corroder now at 3 strength")
        (is (empty? (remove :broken (:subroutines (refresh hive)))) "Hive is now fully broken"))))
  (testing "Auto-pump first"
    (do-game
      (new-game {:runner {:hand ["Corroder"]}
                 :corp {:hand ["Hive"]}})
      (play-from-hand state :corp "Hive" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (let [cor (get-program state 0)
            hive (get-ice state :hq 0)]
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump" :card (refresh cor)})
        (is (= 3 (:current-strength (refresh cor))) "Corroder now at 3 strength")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh cor)})
        (is (empty? (remove :broken (:subroutines (refresh hive)))) "Hive is now fully broken"))))
  (testing "Auto-pump and break some subs manually first"
    (do-game
      (new-game {:runner {:hand ["Corroder"]}
                 :corp {:hand ["Hive"]}})
      (play-from-hand state :corp "Hive" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (let [cor (get-program state 0)
            hive (get-ice state :hq 0)]
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump" :card (refresh cor)})
        (is (= 3 (:current-strength (refresh cor))) "Corroder is now at 3 strength")
        (card-ability state :runner (refresh cor) 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "Done")
        (is (= 4 (count (remove :broken (:subroutines (refresh hive))))) "Only broken 1 sub")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh cor)})
        (is (empty? (remove :broken (:subroutines (refresh hive)))) "Hive is now fully broken")))))
