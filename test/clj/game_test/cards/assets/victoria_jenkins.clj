(ns game-test.cards.assets.victoria-jenkins
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest victoria-jenkins
  ;; Victoria Jenkins
  (do-game
    (new-game {:corp {:deck ["Victoria Jenkins"]}})
    (play-from-hand state :corp "Victoria Jenkins" "New remote")
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner should have 4 clicks by default")
    (let [victoria (get-content state :remote1 0)]
      (core/rez state :corp victoria)
      (is (= 3 (:click (get-runner))) "Runner should have 3 clicks when Victoria Jenkins is rezzed")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (= 3 (:click (get-runner))) "Runner should have 3 clicks again after trashing Victoria Jenkins")
      (is (= 2 (:agenda-point (get-runner))) "Runner should gain 2 agenda points from trashing Victoria Jenkins")
      (is (= 1 (count (get-scored state :runner))) "Runner should have 1 card in score area")
      (is (zero? (-> (get-corp) :discard count)) "Victoria Jenkins shouldn't go to Archives when trashed"))))
