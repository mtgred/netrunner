(ns game-test.cards.identities.noise-hacker-extraordinaire
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest noise-hacker-extraordinaire
  ;; Noise: Hacker Extraordinaire
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)]}
               :runner {:id "Noise: Hacker Extraordinaire"
                        :deck ["Datasucker" "Cache" "Sure Gamble" (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]}})
    (starting-hand state :runner ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-corp)))) "Corp should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-corp)))) "Corp deck should contain 5 cards")
    (take-credits state :corp)
    (is (zero? (count (:discard (get-corp)))) "Archives started empty")
    (play-from-hand state :runner "Datasucker")
    (is (= 1 (count (:discard (get-corp)))) "Playing virus should cause card to be trashed from R&D")
    (is (= 4 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 1 (count (:discard (get-corp)))) "Playing non-virus should not cause card to be trashed from R&D")
    (core/click-draw state :runner nil)
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Clone Chip")
    (trash-from-hand state :runner "Cache")
    (trash-from-hand state :runner "Sharpshooter")
    (take-credits state :runner)
    ;; playing virus via Clone Chip on Corp's turn should trigger Noise ability
    (let [chip (get-hardware state 0)]
      (card-ability state :runner chip 0)
      (click-card state :runner (find-card "Cache" (:discard (get-runner))))
      (let [ds (get-program state 1)]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing virus via Clone Chip on corp's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
    (let [chip-2 (get-hardware state 0)]
      (card-ability state :runner chip-2 0)
      (click-card state :runner (find-card "Sharpshooter" (:discard (get-runner))))
      (let [ss (get-program state 2)]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing non-virus via Clone Chip on corp's turn should not trigger Noise ability")))
