(ns game-test.cards.assets.urban-renewal
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest urban-renewal
  ;; Urban renewal meat damage
  (do-game
    (new-game {:corp {:deck ["Urban Renewal"]}
               :runner {:deck [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]}})
    ;; Corp turn 1, install and rez urban renewal
    (play-from-hand state :corp "Urban Renewal" "New remote")
    (let [ur (get-content state :remote1 0)]
      (core/rez state :corp (refresh ur))
      (take-credits state :corp)
      ;; Runner turn 1, creds
      (is (= 3 (get-counters (refresh ur) :power)))
      (take-credits state :runner)
      ;; Corp turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :corp)
      ;; Runner turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :runner)
      ;; Corp turn 3
      (is (= 1 (get-counters (refresh ur) :power)))
      (take-credits state :corp)
      ;; Runner turn 3
      (is (zero? (count (:discard (get-corp)))) "Nothing in Corp trash")
      (is (zero? (count (:discard (get-runner)))) "Nothing in Runner trash")
      (take-credits state :runner)
      ;; Corp turn 4 - damage fires
      (is (= 1 (count (:discard (get-corp)))) "Urban Renewal got trashed")
      (is (= 4 (count (:discard (get-runner)))) "Urban Renewal did 4 meat damage"))))
