(ns game-test.cards.identities.grndl-power-unleashed
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))
  (testing "vs Valencia - only 1 bad pub at start"
    (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity"))))
