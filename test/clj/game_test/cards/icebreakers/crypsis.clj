(ns game-test.cards.icebreakers.crypsis
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest crypsis
  ;; Crypsis - Loses a virus counter after encountering ice it broke
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Crypsis" 2)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-program state 0)]
      (card-ability state :runner crypsis 2)
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-continue state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-program state 0)]
      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has nil virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))))
