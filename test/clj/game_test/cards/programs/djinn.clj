(ns game-test.cards.programs.djinn
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest djinn
  ;; Djinn
  (testing "Hosted Chakana does not disable advancing agendas. Issue #750"
    (do-game
      (new-game {:corp {:deck ["Priority Requisition"]}
                 :runner {:deck ["Djinn" "Chakana"]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Djinn")
      (let [djinn (get-program state 0)
            agenda (get-content state :remote1 0)]
        (is agenda "Agenda was installed")
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (let [chak (first (:hosted (refresh djinn)))]
          (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
          ;; manually add 3 counters
          (core/add-counter state :runner (first (:hosted (refresh djinn))) :virus 3)
          (take-credits state :runner 2)
          (core/advance state :corp {:card agenda})
          (is (= 1 (get-counters (refresh agenda) :advancement)) "Agenda was advanced")))))
  (testing "Host a non-icebreaker program"
    (do-game
      (new-game {:runner {:deck ["Djinn" "Chakana"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (is (= 3 (core/available-mu state)))
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (is (= 3 (core/available-mu state)) "No memory used to host on Djinn")
        (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
        (is (= 1 (:credit (get-runner))) "Full cost to host on Djinn"))))
  (testing "Tutor a virus program"
    (do-game
      (new-game {:runner {:deck ["Djinn" "Parasite"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand after moving Parasite to deck")
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 0)
        (click-prompt state :runner (find-card "Parasite" (:deck (get-runner))))
        (is (= "Parasite" (:title (first (:hand (get-runner))))) "Djinn moved Parasite to hand")
        (is (= 2 (:credit (get-runner))) "1cr to use Djinn ability")
        (is (= 2 (:click (get-runner))) "1click to use Djinn ability")))))
