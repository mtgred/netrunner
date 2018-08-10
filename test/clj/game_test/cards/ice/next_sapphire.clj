(ns game-test.cards.ice.next-sapphire
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest next-sapphire
  ;; NEXT Sapphire
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["NEXT Bronze" "NEXT Sapphire" (qty "Ice Wall" 100)]}})
      (starting-hand state :corp ["NEXT Bronze" "NEXT Sapphire" "Ice Wall" "Ice Wall"])
      (dotimes [_ 5]
        (core/move state :corp (find-card "Ice Wall" (:deck (get-corp))) :discard))
      (core/gain state :corp :credit 100)
      (play-from-hand state :corp "NEXT Bronze" "HQ")
      (play-from-hand state :corp "NEXT Sapphire" "R&D")
      (let [bronze (get-ice state :hq 0)
            sapphire (get-ice state :rd 0)]
        (core/rez state :corp sapphire)
        (take-credits state :corp)
        (run-on state "R&D")
        (let [hand (count (:hand (get-corp)))
              deck (count (:deck (get-corp)))]
          (card-subroutine state :corp sapphire 0)
          (is (= 1 (-> (get-corp) :prompt first :choices :number)))
          (click-prompt state :corp "1")
          (is (= (inc hand) (count (:hand (get-corp)))) "Corp should draw 1 card from R&D")
          (is (= (dec deck) (count (:deck (get-corp)))) "R&D should lose 1 card"))
        (let [hand (count (:hand (get-corp)))
              trash (count (:discard (get-corp)))]
          (card-subroutine state :corp sapphire 1)
          (click-card state :corp (find-card "Ice Wall" (:discard (get-corp))))
          (is (= (inc hand) (count (:hand (get-corp)))) "Corp should draw 1 card from Archives")
          (is (= (dec trash) (count (:discard (get-corp)))) "Archives should lose 1 card"))
        (let [hand (count (:hand (get-corp)))
              deck (count (:deck (get-corp)))
              num-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
          (card-subroutine state :corp sapphire 2)
          (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
          (is (= (dec hand) (count (:hand (get-corp)))) "Corp should add 1 card from HQ to R&D")
          (is (= (inc deck) (count (:deck (get-corp)))) "R&D should gain 1 card")
          (is (= (inc num-shuffles) (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle"))
        (core/rez state :corp bronze)
        (card-subroutine state :corp sapphire 0)
        (is (= 2 (-> (get-corp) :prompt first :choices :number)) "2 rezzed NEXT ice increases choice total"))))
  (testing "Should shuffle even when choosing 0"
    (do-game
      (new-game {:corp {:deck ["NEXT Sapphire" (qty "Ice Wall" 100)]}})
      (starting-hand state :corp ["NEXT Sapphire" "Ice Wall"])
      (play-from-hand state :corp "NEXT Sapphire" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [sapphire (get-ice state :hq 0)
            hand (count (:hand (get-corp)))
            deck (count (:deck (get-corp)))
            num-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (core/rez state :corp sapphire)
        (card-subroutine state :corp sapphire 2)
        (click-prompt state :corp "Done")
        (is (= hand (count (:hand (get-corp)))) "Nothing selected so HQ shouldn't change")
        (is (= deck (count (:deck (get-corp)))) "Nothing selected so R&D shouldn't change")
        (is (= (inc num-shuffles) (count (core/turn-events state :corp :corp-shuffle-deck)))
            "Corp should shuffle even when selecting nothing")))))
