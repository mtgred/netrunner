(ns game-test.cards.agendas.mandatory-upgrades
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mandatory-upgrades
  ;; Mandatory Upgrades
  (testing "Gain an additional click"
    (do-game
      (new-game {:corp {:deck ["Mandatory Upgrades"
                               "Melange Mining Corp."]}})
      (play-and-score state "Mandatory Upgrades")
      (is (= 2 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Melange Mining Corp." "New remote")
      (let [mmc (get-content state :remote2 0)]
        (core/rez state :corp mmc)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 4 (:click (get-corp))))
        (card-ability state :corp mmc 0)
        (is (= 1 (:click (get-corp)))))))
  (testing "Lose additional click if sacrificed"
    (do-game
      (new-game {:corp {:deck ["Mandatory Upgrades"
                               "Archer"]}})
      (play-and-score state "Mandatory Upgrades")
      (is (= 2 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Archer" "HQ")
      (take-credits state :corp)
      (take-credits state :runner)
      (let [arc (get-ice state :hq 0)
            mu (get-scored state :corp 0)]
        (is (= 4 (:click (get-corp))) "Corp should start turn with 4 clicks")
        (core/rez state :corp arc)
        (click-card state :corp (refresh mu))
        (is (= 3 (:click (get-corp))) "Corp should lose 1 click on agenda sacrifice")))))
