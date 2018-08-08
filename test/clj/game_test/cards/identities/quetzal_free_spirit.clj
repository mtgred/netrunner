(ns game-test.cards.identities.quetzal-free-spirit
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest quetzal-free-spirit
  ;; Quetzal
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
               :runner {:id "Quetzal: Free Spirit"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [q (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)
          qdef (core/card-def (get-in @state [:runner :identity]))
          qmsg (get-in qdef [:abilities 0 :msg])]
      (core/rez state :corp iwall)
      (card-ability state :runner q 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (not (last-log-contains? state qmsg)) "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (core/jack-out state :runner nil))))
