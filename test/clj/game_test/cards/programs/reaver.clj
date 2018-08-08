(ns game-test.cards.programs.reaver
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reaver
  ;; Reaver - Draw a card the first time you trash an installed card each turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Reaver" (qty "Fall Guy" 5)]}})
      (starting-hand state :runner ["Reaver" "Fall Guy"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Reaver")
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 [Credits] to trash") ; Trash PAD campaign
      (is (= 2 (count (:hand (get-runner)))) "Drew a card from trash of corp card")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Fall Guy")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      ; No draw from Fall Guy trash as Reaver already fired this turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      (take-credits state :runner)
      ; Draw from Fall Guy trash on corp turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")))
  (testing "with Freelance Coding Construct - should not draw when trash from hand #2671"
    (do-game
      (new-game {:runner {:deck [(qty "Reaver" 9) "Imp" "Snitch" "Freelance Coding Contract"]}})
      (starting-hand state :runner ["Reaver" "Imp" "Snitch" "Freelance Coding Contract"])
      (take-credits state :corp)
      (play-from-hand state :runner "Reaver")
      (is (= 3 (count (:hand (get-runner)))) "Four cards in hand")
      (is (= 3 (:credit (get-runner))) "3 credits")
      (play-from-hand state :runner "Freelance Coding Contract")
      (click-card state :runner "Snitch")
      (click-card state :runner "Imp")
      (click-prompt state :runner "Done")
      (is (= 7 (:credit (get-runner))) "7 credits - FCC fired")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand"))))
