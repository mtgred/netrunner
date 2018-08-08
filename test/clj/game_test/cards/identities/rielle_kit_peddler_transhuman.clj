(ns game-test.cards.identities.rielle-kit-peddler-transhuman
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rielle-kit-peddler-transhuman
  ;; Rielle "Kit" Peddler - Give ICE Code Gate
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
               :runner {:id "Rielle \"Kit\" Peddler: Transhuman"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [k (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)]
      (core/rez state :corp iwall)
      (card-ability state :runner k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))
