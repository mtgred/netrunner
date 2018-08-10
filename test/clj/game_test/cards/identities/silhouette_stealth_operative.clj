(ns game-test.cards.identities.silhouette-stealth-operative
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest silhouette-stealth-operative
  ;; Silhouette
  (testing "Expose trigger ability resolves completely before access. Issue #2173"
    (do-game
      (new-game {:corp {:deck ["Psychic Field" (qty "Fetal AI" 10)]}
                 :runner {:id "Silhouette: Stealth Operative"
                          :deck ["Feedback Filter" "Inside Job"]}})
      (starting-hand state :corp ["Psychic Field" "Fetal AI"])
      (play-from-hand state :corp "Psychic Field" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let [psychic (get-content state :remote1 0)
            ff (get-hardware state 0)]
        (run-empty-server state :hq)
        (is (:run @state) "On successful run trigger effects")
        (click-card state :runner psychic)
        (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
        (click-prompt state :corp "2 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (card-ability state :runner ff 0)
        (click-prompt state :runner "Done")
        (is (zero? (:credit (get-runner))) "Runner has no more credits left")
        (is (= 1 (count (:hand (get-runner)))) "Prevented 1 net damage")
        (is (empty? (:discard (get-runner))) "No cards discarded")
        (is (:run @state) "On run access phase")
        (click-prompt state :runner "Done")
        (is (empty? (:hand (get-runner))) "Suffered 1 net damage due to accessing Fetal AI")
        (is (= 1 (count (:discard (get-runner)))) "Discarded 1 card due to net damage")
        (is (:run @state) "Resolving access triggers")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "Runner has no credits to be able to steal Fetal AI")
        (is (not (:run @state)) "Run has now ended")
        (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
  (testing "with Temüjin; broken interaction with other successful-run triggers. Issue #1968"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign" (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)]}
                 :runner {:id "Silhouette: Stealth Operative"
                          :deck ["Temüjin Contract" "Desperado"]}})
      (starting-hand state :corp ["Hedge Fund" "PAD Campaign"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "HQ")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "Temüjin Contract")
      (click-card state :runner (get-content state :remote1 0))
      (click-prompt state :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
      (is (= 8 (:credit (get-runner))) "Gained 4cr")
      ;; second run
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 12 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin"))))
