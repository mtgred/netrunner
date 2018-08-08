(ns game-test.cards.identities.weyland-consortium-builder-of-nations
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest weyland-consortium-builder-of-nations
  ;; Builder of Nations
  (testing "1 meat damage per turn at most"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "Hedge Fund" 3)]}})
      (let [bon (get-in @state [:corp :identity])]
        (card-ability state :corp bon 0)
        (click-prompt state :corp "Cancel")
        (is (zero? (count (:discard (get-runner)))) "Runner took no meat damage from BoN")
        (card-ability state :corp bon 0)
        (click-prompt state :corp "Yes")
        (is (= 1 (count (:discard (get-runner)))) "Runner took 1 meat damage from BoN")
        (card-ability state :corp bon 0)
        (is (= 1 (count (:discard (get-runner)))) "Runner took only 1 meat damage from BoN total")
        (is (zero? (count (:prompt (get-corp))))))))
  (testing "2 meat damage from ID ability when The Cleaners is scored"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "The Cleaners" 3) (qty "Ice Wall" 3)]}
                 :runner {:deck [(qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "The Cleaners" "New remote")
      (let [clean (get-content state :remote1 0)]
        (score-agenda state :corp clean)
        (let [bon (get-in @state [:corp :identity])]
          (card-ability state :corp bon 0)
          (click-prompt state :corp "Yes")
          (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from BoN/Cleaners combo"))))))
