(ns game-test.cards.assets.rashida-jaheem
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rashida-jaheem
  ;; Rashida Jaheem
  (testing "when there are enough cards in R&D"
    (do-game
      (new-game {:corp {:deck ["Rashida Jaheem" (qty "Hedge Fund" 3)]}})
      (starting-hand state :corp ["Rashida Jaheem"])
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (let [credits (:credit (get-corp))
            cards (count (:hand (get-corp)))
            rj (get-content state :remote1 0)]
        (card-ability state :corp rj 0)
        (click-prompt state :corp "Yes")
        (is (= (+ 3 credits) (:credit (get-corp))))
        (is (= (+ 3 cards) (count (:hand (get-corp))))))))
  (testing "when there aren't enough cards in R&D"
    (do-game
      (new-game {:corp {:deck ["Rashida Jaheem" (qty "Hedge Fund" 4)]}})
      (starting-hand state :corp ["Rashida Jaheem"])
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (core/draw state :corp)
      (core/draw state :corp)
      (take-credits state :corp)
      (take-credits state :runner)
      (let [credits (:credit (get-corp))
            cards (count (:hand (get-corp)))
            rj (get-content state :remote1 0)]
        (card-ability state :corp rj 0)
        (click-prompt state :corp "Yes")
        (is (= (+ 3 credits) (:credit (get-corp))))
        (is (= (+ 2 cards) (count (:hand (get-corp)))))
        (is (= :runner (:winner @state)) "Runner wins")))))
