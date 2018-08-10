(ns game-test.cards.resources.off-campus-apartment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest off-campus-apartment
  ;; Off-Campus Apartment
  (testing "ability shows a simultaneous resolution prompt when appropriate"
    (do-game
      (new-game {:runner {:deck ["Street Peddler" "Off-Campus Apartment"
                                 "Underworld Contact" (qty "Spy Camera" 6)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Off-Campus Apartment" "Underworld Contact"])
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)]
        (card-ability state :runner oca 0)
        (click-card state :runner (find-card "Underworld Contact" (:hand (get-runner))))
        (is (= 2 (count (:hand (get-runner)))) "Drew a card from OCA")
        (card-ability state :runner oca 0)
        (click-card state :runner (find-card "Street Peddler" (:hand (get-runner))))
        ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
        (is (= 2 (-> (get-runner) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
        (click-prompt state :runner "Off-Campus Apartment")
        (is (= 2 (count (:hand (get-runner)))) "Drew a card from OCA"))))
  (testing "second ability does not break cards that are hosting others, e.g., Street Peddler"
    (do-game
      (new-game {:runner {:deck [(qty "Street Peddler" 2) "Off-Campus Apartment" (qty "Spy Camera" 6)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Street Peddler" "Off-Campus Apartment"])
      (core/move state :runner (find-card "Street Peddler" (:hand (get-runner))) :deck {:front true})
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)]
        (card-ability state :runner oca 0)
        (click-card state :runner (find-card "Street Peddler" (:hand (get-runner))))
        (click-prompt state :runner "Street Peddler")
        (let [ped1 (first (:hosted (refresh oca)))]
          (card-ability state :runner ped1 0)
          (click-prompt state :runner (-> (get-runner) :prompt first :choices second)) ; choose Street Peddler
          (card-ability state :runner (refresh oca) 1)
          (click-card state :runner (get-resource state 1))
          (let [ped2 (first (:hosted (refresh oca)))]
            (card-ability state :runner ped2 0)
            (click-prompt state :runner (-> (get-runner) :prompt first :choices first)) ; choose Spy Camera
            ;; the fact that we got this far means the bug is fixed
            (is (= 1 (count (get-hardware state))) "Spy Camera installed")))))))
