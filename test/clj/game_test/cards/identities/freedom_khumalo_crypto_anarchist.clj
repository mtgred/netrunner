(ns game-test.cards.identities.freedom-khumalo-crypto-anarchist
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest freedom-khumalo-crypto-anarchist
  ;; Freedom Khumalo - Can spend virus counters from other cards to trash accessed cards with play/rez costs
  (testing "Only works with Assets, ICE, Operations, and Upgrades"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (click-prompt state :runner "[Freedom]: Trash card")
                (click-card state :runner (get-program state 0))
                (click-card state :runner (get-program state 0))
                (is (= 1 (count (:discard (get-corp))))
                    (str "Accessed " card " should have been trashed after selecting two virus counters"))))]
      (doall (map fk-test
                  ["Dedicated Response Team"
                   "Consulting Visit"
                   "Builder"
                   "Research Station"]))))
  (testing "Triggers when play/rez cost less than or equal to number of available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (let [cost (->> (get-corp) :hand first :cost)]
                  (click-prompt state :runner "[Freedom]: Trash card")
                  (when (pos? cost)
                    (dotimes [_ cost]
                      (click-card state :runner (get-program state 0))))
                  (is (= 1 (count (:discard (get-corp))))
                      (str "Accessed " card " should have been trashed after selecting " cost " virus counters")))))]
      (doall (map fk-test
                  ["Beanstalk Royalties"
                   "Aggressive Negotiation"
                   "Consulting Visit"
                   "Door to Door"]))))
  (testing "Doesn't trigger when there aren't enough available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (is (= 1 (-> @state :runner :prompt first :choices count)) "Should only have 1 option")
                (is (= "No action" (-> @state :runner :prompt first :choices first)) "Only option should be 'No action'")))]
      (doall (map fk-test
                  ["Archer"
                   "Fire Wall"
                   "Colossus"
                   "Tyrant"]))))
  (testing "Can use multiple programs for virus counter payment"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache" "Virus Breeding Ground"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Virus Breeding Ground")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "[Freedom]: Trash card")
      (click-card state :runner (get-program state 0))
      (click-card state :runner (get-resource state 0))
      (is (= 1 (count (:discard (get-corp))))
          (str "Accessed Dedicated Response Team should have been trashed after selecting 2 virus counters"))))
  (testing "Can use viruses on hosted cards"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Trypano"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (let [iw (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (refresh iw))
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes")
        (run-empty-server state "HQ")
        (click-prompt state :runner "[Freedom]: Trash card")
        (click-card state :runner (-> (refresh iw) :hosted first)))
      (is (= 1 (count (:discard (get-corp)))) "Accessed Ice Wall should be discarded after selecting 1 virus counter")))
  (testing "Doesn't trigger when accessing an Agenda"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= 1 (->> @state :runner :prompt first :choices count)) "Should only have 1 option")
      (is (= "Steal" (-> @state :runner :prompt first :choices first)) "Only option should be 'Steal'")))
(testing "Shows multiple prompts when playing Imp"
  (do-game
    (new-game {:corp {:deck ["Dedicated Response Team"]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Sure Gamble" "Cache" "Imp"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Imp")
    (run-empty-server state "HQ")
    (is (= 4 (-> @state :runner :prompt first :choices count)) "Should have 4 options: Freedom, Imp, Trash, No action")))
(testing "Should return to access prompts when Done is pressed"
  (do-game
    (new-game {:corp {:deck ["Dedicated Response Team"]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Cache"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (run-empty-server state "HQ")
    (is (= 3 (->> @state :runner :prompt first :choices count)) "Should have 3 choices: Freedom, Trash, No action")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner "Done")
    (is (= 3 (-> @state :runner :prompt first :choices count))
        (str "Should go back to access prompts, with 3 choices: Freedom, Trash, No action. "
             "Choices seen: " (-> @state :runner :prompt first :choices)))
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-corp)))) "Card should now be properly discarded")))
(testing "Shouldn't grant additional accesses after trashing accessed card. #3423"
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 10)]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Cache"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (run-empty-server state "R&D")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-corp)))) "Accessed Ice Wall should be discarded now")
    (is (not (:run @state)) "Run ended")))
(testing "Shouldn't give Aumakua additional counters on trash. #3479"
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 10)]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Cache" "Aumakua"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Aumakua")
    (run-empty-server state "R&D")
    (is (zero? (get-counters (get-program state 1) :virus)) "Aumakuma shouldn't have any virus counters yet.")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-corp)))) "Ice Wall should be discarded now")
    (is (zero? (get-counters (get-program state 1) :virus)) "Aumakua doesn't gain any virus counters from trash ability.")
    (is (not (:run @state)) "Run ended")))
(testing "interaction with trash-cost-bonuses, and declining ability once initiated"
  (do-game
    (new-game {:corp {:deck ["The Board"]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Skulljack" "Imp" "Sure Gamble"]}})
    (play-from-hand state :corp "The Board" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner doesn't have enough credits to trash")
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Imp")
    (core/add-counter state :runner (get-program state 0) :virus 5)
    (play-from-hand state :runner "Skulljack")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "The Board should cost 6 to trash")
    (is (= 3 (-> (get-runner) :prompt first :choices count)) "Runner can use Freedom or Imp to trash")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner "Done")
    (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "Skulljack shouldn't trigger a second time")
    (is (= 3 (-> (get-runner) :prompt first :choices count)) "Runner can still use Freedom or Imp the second time around")
    (click-prompt state :runner "[Imp]: Trash card")
    (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board with Imp and gain 2 agenda points"))))
