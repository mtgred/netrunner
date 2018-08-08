(ns game-test.cards.agendas.labyrinthine-servers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest labyrinthine-servers
  ;; Labyrinthine Servers
  (do-game
    (new-game {:corp {:deck [(qty "Labyrinthine Servers" 2)]}})
    (play-and-score state "Labyrinthine Servers")
    (play-and-score state "Labyrinthine Servers")
    (take-credits state :corp)
    (let [ls1 (get-scored state :corp 0)
          ls2 (get-scored state :corp 1)]
      (is (= 2 (get-counters (refresh ls1) :power)))
      (is (= 2 (get-counters (refresh ls2) :power)))
      (testing "Don't use token"
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state) "Jack out prevent prompt")
        (click-prompt state :corp "Done")
        (is (not (:run @state)) "Corp does not prevent the jack out, run ends"))
      (testing "Use token"
        (run-on state "HQ")
        (run-jack-out state)
        (card-ability state :corp ls1 0)
        (card-ability state :corp ls2 0)
        (card-ability state :corp ls1 0)
        (click-prompt state :corp "Done")
        (is (:run @state) "Jack out prevented, run is still ongoing")
        (is (true? (get-in @state [:run :cannot-jack-out])) "Cannot jack out flag is in effect")
        (run-successful state)
        (is (not (:run @state))))
      (testing "one Labyrinthine is empty but the other still has one token, ensure prompt still occurs"
        (is (zero? (get-counters (refresh ls1) :power)))
        (is (= 1 (get-counters (refresh ls2) :power)))
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state))
        (card-ability state :corp ls2 0)
        (click-prompt state :corp "Done")
        (is (true? (get-in @state [:run :cannot-jack-out])))
        (run-successful state)
        (is (not (:run @state))))
      (testing "No more tokens"
        (run-on state "HQ")
        (run-jack-out state)
        (is (not (:run @state)) "No jack out prevent prompt")))))
