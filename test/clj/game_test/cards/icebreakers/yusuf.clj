(ns game-test.cards.icebreakers.yusuf
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
    (new-game {:corp {:deck ["Fire Wall"]}
               :runner {:deck ["Yusuf" "Cache"]}})
    (play-from-hand state :corp "Fire Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Yusuf")
    (play-from-hand state :runner "Cache")
    (let [fire-wall (get-ice state :hq 0)
          yusuf (get-program state 0)
          cache (get-program state 1)]
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (refresh yusuf) :virus)) "Yusuf has 1 virus counter")
      (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
      (is (= 3 (get-counters (refresh cache) :virus)) "Initial Cache virus counters")
      (run-on state "HQ")
      (core/rez state :corp fire-wall)
      (card-ability state :runner yusuf 1) ; match strength
      (click-card state :runner cache)
      (click-card state :runner yusuf)
      (is (= 2 (get-counters (refresh cache) :virus)) "Cache lost 1 virus counter to pump")
      (is (= 5 (:current-strength (refresh yusuf))) "Yusuf strength 5")
      (is (= 0 (get-counters (refresh yusuf) :virus)) "Yusuf lost 1 virus counter to pump")
      (is (empty? (:prompt (get-runner))) "No prompt open")
      (card-ability state :runner yusuf 0)
      (click-card state :runner cache)
      (click-prompt state :runner "Done")
      (is (= 1 (get-counters (refresh cache) :virus)) "Cache lost its final virus counter"))))
