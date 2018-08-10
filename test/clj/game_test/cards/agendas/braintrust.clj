(ns game-test.cards.agendas.braintrust
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest braintrust
  ;; Braintrust
  (do-game
    (new-game {:corp {:deck ["Braintrust" "Ichi 1.0"]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (let [bt (get-content state :remote1 0)]
      (core/add-prop state :corp bt :advance-counter 7)
      (core/score state :corp {:card (refresh bt)})
      (let [scored-bt (get-scored state :corp 0)]
        (is (= 2 (get-counters (refresh scored-bt) :agenda))
            "Scored w/ 4 over-advancements; 2 agenda counters")
        (play-from-hand state :corp "Ichi 1.0" "HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (is (= 2 (:credit (get-corp))) "2c discount to rez Ichi")))))
