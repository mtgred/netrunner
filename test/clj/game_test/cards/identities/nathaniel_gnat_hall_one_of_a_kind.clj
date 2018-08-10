(ns game-test.cards.identities.nathaniel-gnat-hall-one-of-a-kind
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nathaniel-gnat-hall-one-of-a-kind
  ;; Nathaniel "Gnat" Hall: One-of-a-Kind
  (do-game
    (new-game {:runner {:id "Nathaniel \"Gnat\" Hall: One-of-a-Kind"
                        :deck [(qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Did not gain a credit when Gnat is on 3 cards")
    (play-from-hand state :runner "Sure Gamble")
    (take-credits state :runner)
    (let [runner-credits (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (inc runner-credits) (:credit (get-runner)))) "Gained 1 credits when on 2 cards")))
