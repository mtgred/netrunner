(ns game-test.cards.operations.the-all-seeing-i
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-all-seeing-i
  (testing "Counts number of cards if one card is prevented trashed with fall guy"
    (do-game
      (new-game {:corp {:deck ["The All-Seeing I"]}
                 :runner {:deck ["Fall Guy" (qty "Same Old Thing" 2)]}})
      (letfn [(res [] (count (get-in (get-runner) [:rig :resource])))]
        (take-credits state :corp)
        (play-from-hand state :runner "Same Old Thing")
        (play-from-hand state :runner "Fall Guy")
        (play-from-hand state :runner "Same Old Thing")
        (take-credits state :runner)
        (play-from-hand state :corp "The All-Seeing I")
        (is (= 1 (count (:hand (get-corp)))) "Corp could not play All Seeing I when runner was not tagged")
        (core/gain state :runner :tag 1)
        (play-from-hand state :corp "The All-Seeing I")
        (let [fall-guy (get-resource state 1)]
          (card-ability state :runner fall-guy 0))
        (click-prompt state :runner "Done")
        (is (= 1 (res)) "One installed resource saved by Fall Guy")
        (is (= 2 (count (:discard (get-runner)))) "Two cards in heap"))))
  (testing "Checks that All-seeing I does not double-trash hosted cards, trashes hosted cards"
    (do-game
      (new-game {:corp {:deck ["The All-Seeing I"]}
                 :runner {:deck [(qty "Fall Guy" 2) "Off-Campus Apartment"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)
            fg1 (get-in (get-runner) [:hand 0])
            fg2 (get-in (get-runner) [:hand 1])]
        (card-ability state :runner oca 0)
        (click-card state :runner fg1)
        (card-ability state :runner oca 0)
        (click-card state :runner fg2))
      (core/gain state :runner :tag 1)
      (take-credits state :runner)
      (play-from-hand state :corp "The All-Seeing I")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "Done")
      (let  [fall-guy (find-card "Fall Guy" (core/all-active-installed state :runner))]
        (card-ability state :runner fall-guy 0))
      (click-prompt state :runner "Done") ;; This assumes hosted cards get put in trash-list before host
      (is (= 1 (count (core/all-active-installed state :runner))) "One installed card (Off-Campus)")
      (is  (= 2 (count (:discard (get-runner)))) "Two cards in heap")))
  (testing "should not trash Jarogniew Mercs if there are other installed resources"
    (do-game
      (new-game {:corp {:deck [(qty "The All-Seeing I" 4)]}
                 :runner {:deck [(qty "Jarogniew Mercs" 2) (qty "Same Old Thing" 2)]}})
      (letfn [(res [] (count (get-in (get-runner) [:rig :resource])))]
        (take-credits state :corp)
        (play-from-hand state :runner "Same Old Thing")
        (play-from-hand state :runner "Jarogniew Mercs")
        (take-credits state :runner)
        (is (= 2 (res)) "There are two installed resources")
        (play-from-hand state :corp "The All-Seeing I")
        (is (= 1 (res)) "Jarogniew Mercs still installed")
        (play-from-hand state :corp "The All-Seeing I")
        (is (zero? (res)) "There are no installed resources")
        (take-credits state :corp)
        (play-from-hand state :runner "Jarogniew Mercs") ;; Testing if order matters
        (play-from-hand state :runner "Same Old Thing")
        (take-credits state :runner)
        (is (= 2 (res)) "There are two installed resources")
        (play-from-hand state :corp "The All-Seeing I")
        (is (= 1 (res)) "Jarogniew Mercs still installed")
        (play-from-hand state :corp "The All-Seeing I")
        (is (zero? (res)) "There are no installed resources")))))
