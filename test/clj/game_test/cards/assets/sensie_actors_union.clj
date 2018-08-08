(ns game-test.cards.assets.sensie-actors-union
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sensie-actors-union
  ;; Sensie Actors Union
  (do-game
    (new-game {:corp {:deck ["Sensie Actors Union" "Ronin" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Sensie Actors Union" "Ronin"])
    (core/move state :corp (find-card "Ronin" (:hand (get-corp))) :deck {:front true})
    (play-from-hand state :corp "Sensie Actors Union" "New remote")
    (let [sau (get-content state :remote1 0)]
      (core/rez state :corp sau)
      (take-credits state :corp)
      (is (zero? (count (:hand (get-corp)))) "Corp should have no cards in hand before starting turn")
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp sau 0)
      (is (= 3 (count (:hand (get-corp)))) "Corp should draw 3 cards from Sensie Actors Union's ability")
      (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
      (is (= "Ronin" (-> (get-corp) :deck last :title)) "Ronin should be on bottom of deck")
      (core/end-phase-12 state :corp nil)
      (is (= 3 (count (:hand (get-corp)))) "Corp should have 3 cards in hand after putting one on bottom of R&D and mandatory draw")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (nil? (:corp-phase-12 @state)) "Sensie Actors Union doesn't trigger if protected by ice"))))
