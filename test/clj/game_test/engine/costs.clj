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
      (is (= "Corp spends [Click] and pays 0 [Credits] to install ICE protecting HQ." (-> @state :log last :text)) "Install ICE, zero cost")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= "Corp spends [Click] and pays 1 [Credits] to install ICE protecting HQ." (-> @state :log last :text)) "Install ICE, one cost")
      (play-from-hand state :corp "Turtlebacks" "New remote")
      (is (= "Corp spends [Click] to install a card in Server 1 (new remote)." (-> @state :log last :text)) "Install asset, zero cost")
      (play-from-hand state :corp "Ben Musashi" "Server 1")
      (is (= "Corp spends [Click] to install a card in Server 1." (-> @state :log last :text)) "Install upgrade, zero cost")
      (play-from-hand state :corp "Project Beale" "New remote")
      (is (= "Corp spends [Click] to install a card in Server 2 (new remote)." (-> @state :log last :text)) "Install agenda, zero cost")
      (play-from-hand state :corp "Beanstalk Royalties")
      (is (= "Corp spends [Click] and pays 0 [Credits] to play Beanstalk Royalties." (-> @state :log reverse second :text)) "Play operation, zero cost")
      (play-from-hand state :corp "Hedge Fund")
      (is (= "Corp spends [Click] and pays 5 [Credits] to play Hedge Fund." (-> @state :log reverse second :text)) "Play operation, five cost")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Diesel")
      (is (= "Runner spends [Click] and pays 0 [Credits] to play Diesel." (-> @state :log reverse second :text)) "Play event, zero cost")
      (play-from-hand state :runner "Sure Gamble")
      (is (= "Runner spends [Click] and pays 5 [Credits] to play Sure Gamble." (-> @state :log reverse second :text)) "Play event, five cost")
      (play-from-hand state :runner "Clot")
      (is (= "Runner spends [Click] and pays 2 [Credits] to install Clot." (-> @state :log last :text)) "Install program, two cost")
      (play-from-hand state :runner "Misdirection")
      (is (= "Runner spends [Click] and pays 0 [Credits] to install Misdirection." (-> @state :log last :text)) "Install program, zero cost")
      (play-from-hand state :runner "Career Fair")
      (is (= "Runner spends [Click] and pays 0 [Credits] to play Career Fair." (-> @state :log last :text)) "Play Career Fair, zero cost")
      (click-card state :runner (find-card "Daily Casts" (:hand (get-runner))))
      (is (= "Runner pays 0 [Credits] to install Daily Casts." (-> @state :log last :text)) "Choose Daily cast, zero cost install")
      (play-from-hand state :runner "Daily Casts")
      (is (= "Runner spends [Click] and pays 3 [Credits] to install Daily Casts." (-> @state :log last :text)) "Install resource, three cost")
      (run-on state :archives)
      (is (= "Runner spends [Click] to make a run on Archives." (-> @state :log last :text)) "Initiate run, zero cost"))))

