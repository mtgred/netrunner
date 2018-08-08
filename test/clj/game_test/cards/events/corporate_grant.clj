(ns game-test.cards.events.corporate-grant
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest corporate-grant
  ;; Corporate "Grant" - First time runner installs a card, the corp loses 1 credit
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Corporate \"Grant\"" (qty "Daily Casts" 2)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Corporate \"Grant\"")
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (play-from-hand state :runner "Daily Casts")
      (is (= 7 (:credit (get-corp))) "Corp loses 1 credit")
      (play-from-hand state :runner "Daily Casts")
      (is (empty? (:hand (get-runner))) "Played all cards in hand")
      (is (= 7 (:credit (get-corp))) "Corp doesn't lose 1 credit")))
  (testing "with Hayley Kaplan. Issue #3162"
    (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :deck ["Corporate \"Grant\"" (qty "Clone Chip" 2)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Corporate \"Grant\"")
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (play-from-hand state :runner "Clone Chip")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Clone Chip" (:hand (get-runner))))
      (is (= 7 (:credit (get-corp))) "Corp only loses 1 credit"))))
