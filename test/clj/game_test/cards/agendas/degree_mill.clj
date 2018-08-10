(ns game-test.cards.agendas.degree-mill
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest degree-mill
  ;; Degree Mill
  (testing "Basic behavior"
    (do-game
      (new-game {:corp {:deck [(qty "Degree Mill" 2)]}
                 :runner {:deck ["Ice Analyzer" "All-nighter" "Hunting Grounds"]}})
      (play-from-hand state :corp "Degree Mill" "New remote")
      (take-credits state :corp)
      (is (= 0 (count (:deck (get-runner)))) "Runner starts with empty deck")
      (run-on state "Server 1")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (= 0 (:agenda-point (get-runner))) "Runner stole Degree Mill with no installed cards")
      (play-from-hand state :runner "Ice Analyzer")
      (play-from-hand state :runner "All-nighter")
      (let [ia (get-resource state 0)
            an (get-resource state 1)]
        (run-on state "Server 1")
        (run-successful state)
        (click-prompt state :runner "Pay shuffling 2 installed cards into the stack to steal")
        (click-card state :runner ia)
        (click-card state :runner an)
        (is (= 3 (:agenda-point (get-runner))) "Runner failed to steal Degree Mill")
        (is (empty? (get-in @state [:runner :rig :resource])) "Degree Mill didn't remove installed cards")
        (is (= 2 (count (:deck (get-runner)))) "Degree Mill didn't put cards back in deck"))
      (take-credits state :runner)
      ;; Checking if facedowns work as well
      (play-from-hand state :corp "Degree Mill" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (let [hg (get-resource state 0)]
        (run-on state "Server 2")
        (run-successful state)
        (click-prompt state :runner "No action")
        (is (= 3 (:agenda-point (get-runner))) "Runner stole Degree Mill with single card")
        (card-ability state :runner hg 1)
        (is (= 2 (count (get-in (get-runner) [:rig :facedown]))) "Hunting Ground did not install cards facedown")
        (is (empty? (:deck (get-runner))) "Hunting Grounds did not remove cards from deck")
        (let [fd1 (get-runner-facedown state 0)
              fd2 (get-runner-facedown state 1)]
          (run-on state "Server 2")
          (run-successful state)
          (click-prompt state :runner "Pay shuffling 2 installed cards into the stack to steal")
          (click-card state :runner fd1)
          (click-card state :runner fd2)
          (is (= 6 (:agenda-point (get-runner))) "Runner failed to steal Degree Mill with facedown cards")
          (is (empty? (get-in (get-runner)  [:rig :facedown])) "Degree Mill didn't remove facedown cards")
          (is (= 2 (count (:deck (get-runner)))) "Degree Mill didn't put cards back in deck")))))
  (testing "Multiple steal costs"
    (do-game
      (new-game {:corp {:deck [(qty "Degree Mill" 1) (qty "Strongbox" 1)]}
                 :runner {:deck [(qty "Ice Analyzer" 3) (qty "All-nighter" 3)]}})
      (play-from-hand state :corp "Degree Mill" "New remote")
      (play-from-hand state :corp "Strongbox" "Server 1")
      (let [dm (get-content state :remote1 0)
            sb (get-content state :remote1 1)]
        (core/rez state :corp sb)
        (take-credits state :corp)
        (play-from-hand state :runner "Ice Analyzer")
        (play-from-hand state :runner "All-nighter")
        (run-empty-server state :remote1)
        (click-card state :runner (refresh dm))
        (click-prompt state :runner "Pay to steal")
        (is (= 1 (:click (get-runner))) "Runner should start with 1 remaining click")
        (click-prompt state :runner "[Click]")
        (is (zero? (:click (get-runner))) "Runner should have spent a click")
        (is (= 2 (count (get-in @state [:runner :rig :resource]))) "Runner starts with 2 resources")
        (click-prompt state :runner "shuffling 2 installed cards into the stack")
        (click-card state :runner (get-resource state 1))
        (click-card state :runner (get-resource state 0))
        (is (empty? (get-resource state)) "Degree Mill removed installed cards")
        (is (not-empty (get-scored state :runner)) "Runner stole an agenda")))))
