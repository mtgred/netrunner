(ns game-test.cards.agendas.napd-contract
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest napd-contract
  ;; NAPD Contract
  (testing "basic test"
    (do-game
      (new-game {:corp {:deck ["NAPD Contract"]}})
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (advance state napd 2)
        (take-credits state :corp)
        (core/lose state :runner :credit 2)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal NAPD Contract")
        (is (= 3 (:credit (get-runner))) "Runner couldn't afford to steal, so no credits spent")
        (take-credits state :runner)
        (core/gain state :corp :bad-publicity 1)
        (advance state napd 2)
        (core/score state :corp {:card (refresh napd)})
        (is (some? (get-content state :remote1 0))
            "Corp can't score with 4 advancements because of BP")
        (advance state napd)
        (core/score state :corp {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-corp))) "Scored NAPD for 2 points after 5 advancements"))))
  (testing "scoring requirement increases with bad publicity from Corporate Scandal"
    (do-game
      (new-game {:corp {:deck ["NAPD Contract"]}
                 :runner {:deck ["Corporate Scandal"]}})
      (play-from-hand state :corp "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (advance state napd 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Corporate Scandal")
        (take-credits state :runner)
        (advance state napd 2)
        (core/score state :corp {:card (refresh napd)})
        (is (some? (get-content state :remote1 0))
            "Corp can't score with 4 advancements because of BP")
        (advance state napd)
        (core/score state :corp {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-corp))) "Scored NAPD for 2 points after 5 advancements")))))
