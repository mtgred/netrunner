(ns game-test.cards.identities.the-outfit-family-owned-and-operated
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-outfit-family-owned-and-operated
  ;; The Outfit - Gain 3 whenever you take at least 1 bad publicity
  (testing "basic test"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Hostile Takeover" "Profiteering"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 1 (:bad-publicity (get-corp))) "Take 1 bad publicity")
      (is (= 15 (:credit (get-corp))) "Corp should gain 10 credits")
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (click-prompt state :corp "3")  ;; Take 3 bad publicity from Profiteering, gain 15
      (is (= 4 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity")
      (is (= 33 (:credit (get-corp))) "Corp should gain 18 credits")))
  (testing "with Profiteering - Only gain 3 credits when taking more than 1 bad publicity in a single effect"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Profiteering"]}})
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "3")
      (is (= 3 (:bad-publicity (get-corp))) "Take 3 bad publicity")
      (is (= 23 (:credit (get-corp))) "Gain 15 from Profiteering + 3 from The Outfit")))
  (testing "vs Valencia - 1 bad pub at start means 5 credits to start with (does not _gain_ BP)"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Hostile Takeover"]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 1 (:bad-publicity (get-corp))) "The Outfit starts with 1 bad publicity")
      (is (= 5 (:credit (get-corp))) "The Outfit starts with 8 credits")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 2 (:bad-publicity (get-corp))) "Take 1 bad publicity")
      (is (= (+ 5 7 3) (:credit (get-corp))) "Gain 7 from Hostile Takeover + 3 from The Outfit"))))
