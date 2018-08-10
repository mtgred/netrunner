(ns game-test.cards.operations.high-profile-target
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest high-profile-target
  (testing "when the runner has no tags"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))
  (testing "when the runner has one tag"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 3 (count (:hand (get-runner)))) "Runner has 3 cards in hand")))
  (testing "when the runner has two tags"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 6)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (core/gain state :runner :tag 2)
      (play-from-hand state :corp "High-Profile Target")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))
  (testing "When the runner has three tags, gg"
    (do-game
      (new-game {:corp {:deck [(qty "High-Profile Target" 10)]}})
      (core/gain state :runner :tag 3)
      (play-from-hand state :corp "High-Profile Target")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
