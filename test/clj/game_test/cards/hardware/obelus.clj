(ns game-test.cards.hardware.obelus
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Obelus" "Nerve Agent"
                                 (qty "Sure Gamble" 3) (qty "Cache" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus" "Nerve Agent"])
      (core/gain state :runner :credit 10 :click 3)
      (play-from-hand state :runner "Nerve Agent")
      (let [nerve (get-program state 0)]
        (run-empty-server state :hq)
        (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
        (click-prompt state :runner "No action")
        (play-from-hand state :runner "Obelus")
        (core/gain state :runner :tag 1)
        (is (= 6 (core/hand-size state :runner)) "Max hand size is 6")
        (core/lose state :runner :tag 1)
        (is (= 5 (core/hand-size state :runner)) "Max hand size is 5")
        (run-empty-server state :hq)
        (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
        (click-prompt state :runner "1")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (is (empty? (:hand (get-runner))) "No cards drawn by Obelus, already had successful HQ run")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :hq)
        (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
        (click-prompt state :runner "2")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards"))))
  (testing "running and trashing Crisium Grid makes run neither successful/unsuccessful"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Crisium Grid"]}
                 :runner {:deck ["Obelus" (qty "Sure Gamble" 3)]}})
      (starting-hand state :corp ["Crisium Grid"])
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (core/rez state :corp (get-content state :rd 0))
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus"])
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Crisium Grid")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "No action")
      (is (empty? (:hand (get-runner))) "Crisium Grid blocked successful run")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run")))
  (testing "using Hades Shard during run to increase draw"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3)]}
                 :runner {:deck ["Obelus" "Hades Shard"
                                 (qty "Sure Gamble" 3) (qty "Cache" 3)]}})
      (starting-hand state :corp ["Hedge Fund" "Hedge Fund"])
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus" "Hades Shard"])
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Obelus")
      (play-from-hand state :runner "Hades Shard")
      (run-empty-server state "R&D")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "No action")
      (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards")))
  (testing "running a remote server first doesn't block card draw"
    (do-game
      (new-game {:corp {:deck ["Urban Renewal" "Hedge Fund"]}
                 :runner {:deck ["Obelus" (qty "Sure Gamble" 3)]}})
      (starting-hand state :corp ["Urban Renewal"])
      (play-from-hand state :corp "Urban Renewal" "New remote")
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus"])
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run"))))
