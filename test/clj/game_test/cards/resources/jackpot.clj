(ns game-test.cards.resources.jackpot
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jackpot
  ;; Jackpot! - whenever a card enters your score area, trash Jackpot to pull off credits
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Braintrust"]}
                 :runner {:deck ["Jackpot!"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Jackpot!")
      (let [jak (get-resource state 0)]
        (is (zero? (get-counters (refresh jak) :credit)) "Jackpot! starts with 0 credits")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn (2nd turn)")
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (is (= 2 (:agenda-point (get-runner))) "Runner steals Braintrust")
        (click-prompt state :runner "Yes")
        (is (= 12 (:credit (get-runner))) "Runner starts with 12 credits")
        (click-prompt state :runner "2")
        (is (= 14 (:credit (get-runner))) "Runner gains 2 credits")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed"))))
  (testing "should fire when moving agendas from Film Critic to scored area"
    (do-game
      (new-game {:corp {:deck ["Project Vitruvius"]}
                 :runner {:deck ["Jackpot!" "Film Critic"]}})
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (play-from-hand state :runner "Jackpot!")
      (let [fc (get-resource state 0)
            jak (get-resource state 1)]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (take-credits state :corp)
        (card-ability state :runner fc 0)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "1")
        (is (= 1 (count (:scored (get-runner)))) "Moved agenda to scored area")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed")
        (is (empty? (:hosted (refresh fc))) "Removed agenda hosted on FC"))))
  (testing "should fire when trashing Chairman Hiro"
    (do-game
      (new-game {:corp {:deck ["Chairman Hiro"]}
                 :runner {:deck ["Jackpot!"]}})
      (play-from-hand state :corp "Chairman Hiro" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Jackpot!")
      (let [jak (get-resource state 0)]
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 6 [Credits] to trash") ;trash CH
        (click-prompt state :runner "Yes") ;trash Jackpot!
        (click-prompt state :runner "1")
        (is (= 3 (:credit (get-runner))) "Runner gains 1 credit")
        (is (= 1 (count (:scored (get-runner)))) "Chairman Hiro in score area")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed")))))
