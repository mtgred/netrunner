(ns game-test.cards.hardware.hijacked-router
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hijacked-router
  ;; Hijacked Router
  (testing "Run on Archives"
    (do-game
      (new-game {:runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp ends turn with 8 credits")
      (play-from-hand state :runner "Hijacked Router")
      (run-empty-server state :archives)
      (is (not-empty (get-hardware state)) "Hijacked Router installed")
      (is (-> (get-runner) :prompt first :card :title (= "Hijacked Router")) "Prompt for using Hijacked Router")
      (click-prompt state :runner "Yes")
      (is (empty? (get-hardware state)) "Hijacked Router is not installed")
      (is (find-card "Hijacked Router" (:discard (get-runner))) "Hijacked Router was trashed")
      (is (= 5 (:credit (get-corp))) "Corp lost 3 credits")
      (is (not (:run @state)) "Run is finished")))
  (testing "Run on HQ"
    (do-game
      (new-game {:runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp ends turn with 8 credits")
      (play-from-hand state :runner "Hijacked Router")
      (run-empty-server state :hq)
      (is (not-empty (get-hardware state)) "Hijacked Router installed")
      (is (-> (get-runner) :prompt first :card :title (= "Hedge Fund")) "No prompt to use Hijacked Router")
      (is (not-empty (get-hardware state)) "Hijacked Router is installed")
      (is (not (find-card "Hijacked Router" (:discard (get-runner)))) "Hijacked Router was not trashed")
      (is (= 8 (:credit (get-corp))) "Corp has not lost 3 credits")))
  (testing "Credit loss on server creation"
    (do-game
      (new-game {:corp {:deck ["Elective Upgrade"]}
                 :runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hijacked Router")
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Corp starts turn with 8 credits")
      (play-from-hand state :corp "Elective Upgrade" "New remote")
      (is (= 7 (:credit (get-corp))) "Corp lost 1 credit from server creation"))))
