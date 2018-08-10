(ns game-test.cards.agendas.hyperloop-extension
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hyperloop-extension
  ;; Hyperloop Extension
  (testing "Score"
    (do-game
      (new-game {:corp {:deck ["Hyperloop Extension"]}})
      (play-from-hand state :corp "Hyperloop Extension" "New remote")
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 8 (:credit (get-corp))) "Corp gains 3 credits")))
  (testing "Steal"
    (do-game
      (new-game {:corp {:deck ["Hyperloop Extension"]}})
      (play-from-hand state :corp "Hyperloop Extension" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 7 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :runner "Steal")
      (is (= 10 (:credit (get-corp))) "Corp gains 3 credits"))))
