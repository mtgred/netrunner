(ns game-test.cards.identities.strategic-innovations-future-forward
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward
  (do-game
    (new-game {:corp {:id "Strategic Innovations: Future Forward"
                      :deck [(qty "Hedge Fund" 2) (qty "Eli 1.0" 2) (qty "Crick" 2)]}})
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Crick" "New remote")
    (let [i1 (get-ice state :remote1 0)
          i2 (get-ice state :remote2 0)]
      (take-credits state :corp 0)
      (take-credits state :runner)
      (core/rez state :corp i1)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (count (:prompt (get-corp)))) "Corp prompted to trigger Strategic Innovations")
      (click-card state :corp (first (:discard (get-corp))))
      (is (empty? (:discard (get-corp))) "Hedge Fund moved back to R&D")
      (take-credits state :corp)
      (core/rez state :corp i2)
      (take-credits state :runner)
      (is (zero? (count (:prompt (get-corp))))
          "Corp not prompted to trigger Strategic Innovations"))))
