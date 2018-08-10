(ns game-test.cards.agendas.armored-servers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest armored-servers
  ;; Armored Servers
  (do-game
    (new-game {:corp {:deck ["Armored Servers"]}})
    (play-and-score state "Armored Servers")
    (let [as-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh as-scored) :agenda)) "Should start with 1 agenda counters")
      (take-credits state :corp)
      (run-on state "HQ")
      (card-ability state :corp as-scored 0)
      (is (last-log-contains? state "make the Runner trash") "Should only write to log"))))
