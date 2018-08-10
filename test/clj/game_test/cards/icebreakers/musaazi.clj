(ns game-test.cards.icebreakers.musaazi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest musaazi
  ;; Musaazi gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
    (new-game {:corp {:deck ["Lancelot"]}
               :runner {:deck ["Musaazi" "Imp"]}})
    (play-from-hand state :corp "Lancelot" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Musaazi")
    (play-from-hand state :runner "Imp")
    (let [lancelot (get-ice state :hq 0)
          musaazi (get-program state 0)
          imp (get-program state 1)]
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (refresh musaazi) :virus)) "Musaazi has 1 virus counter")
      (is (= 1 (:current-strength (refresh musaazi))) "Initial Musaazi strength")
      (is (= 2 (get-counters (refresh imp) :virus)) "Initial Imp virus counters")
      (run-on state "HQ")
      (core/rez state :corp lancelot)
      (card-ability state :runner musaazi 1) ; match strength
      (click-card state :runner imp)
      (is (= 1 (get-counters (refresh imp) :virus)) "Imp lost 1 virus counter to pump")
      (is (= 2 (:current-strength (refresh musaazi))) "Musaazi strength 2")
      (is (empty? (:prompt (get-runner))) "No prompt open")
      (card-ability state :runner musaazi 0)
      (click-card state :runner musaazi)
      (click-card state :runner imp)
      (click-prompt state :runner "Done")
      (is (= 0 (get-counters (refresh imp) :virus)) "Imp lost its final virus counter")
      (is (= 0 (get-counters (refresh imp) :virus)) "Musaazi lost its virus counter"))))
