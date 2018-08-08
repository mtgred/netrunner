(ns game-test.cards.identities.andromeda-dispossessed-ristie
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest andromeda-dispossessed-ristie
  ;; Andromeda - 9 card starting hand, 1 link
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 1 (:link (get-runner))) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))
  (testing "9 card starting hand after mulligan"
    (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}}
                {:mulligan :runner})
      (is (= 1 (:link (get-runner))) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))
  (testing "should not grant Palana credits"
    (do-game
      (new-game {:corp {:id "Pālanā Foods: Sustainable Growth"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 5 (:credit (get-corp))) "Palana does not gain credit from Andromeda's starting hand"))))
