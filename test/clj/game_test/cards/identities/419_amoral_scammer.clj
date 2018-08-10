(ns game-test.cards.identities.419-amoral-scammer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ^{:card-title "419-amoral-scammer"}
  FourHundredAndNineTeen-amoral-scammer
  ;; 419
  (testing "basic test: Amoral Scammer - expose first installed card unless corp pays 1 credit"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck ["PAD Campaign" "The Cleaners" (qty "Pup" 3) "Oaktown Renovation"]}
                 :runner {:id "419: Amoral Scammer"}})
      (is (= 5 (:credit (get-corp))) "Starts with 5 credits")
      (play-from-hand state :corp "Pup" "HQ")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "Yes")
      (is (= 4 (:credit (get-corp))) "Pays 1 credit to not expose card")
      (play-from-hand state :corp "Pup" "HQ")
      (is (empty? (:prompt (get-runner))) "No option on second install")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Archives")
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-corp))) "No prompt if Runner chooses No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "The Cleaners" "New remote")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (is (last-log-contains? state "exposes The Cleaners") "Installed card was exposed")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (empty? (:prompt (get-corp))) "Cannot expose faceup agendas")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/lose state :corp :credit (:credit (get-corp)))
      (is (zero? (:credit (get-corp))) "Corp has no credits")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (click-prompt state :runner "Yes")
      (is (empty? (:prompt (get-corp))) "No prompt if Corp has no credits")
      (is (last-log-contains? state "exposes PAD Campaign") "Installed card was exposed")))
  (testing "Verify expose can be blocked"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck ["Underway Grid" "Pup"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Underway Grid" "New remote")
      (click-prompt state :runner "No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Server 1")
      (click-prompt state :runner "Yes")
      (let [ug (get-in @state [:corp :servers :remote1 :content 0])]
        (core/rez state :corp ug)
        (click-prompt state :corp "No")
        (is (last-log-contains? state "uses Underway Grid to prevent 1 card from being exposed") "Exposure was prevented"))))
  (testing "Ixodidae shouldn't trigger off 419's ability"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:id "419: Amoral Scammer"
                          :deck ["Ixodidae"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Ixodidae")
      (take-credits state :runner)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (let [corp-credits (:credit (get-corp))
            runner-credits (:credit (get-runner))]
        (click-prompt state :runner "Yes")
        (click-prompt state :corp "Yes")
        (is (= 1 (- corp-credits (:credit (get-corp)))) "Should lose 1 credit from 419 ability")
        (is (zero? (- runner-credits (:credit (get-runner)))) "Should not gain any credits from Ixodidae")))))
