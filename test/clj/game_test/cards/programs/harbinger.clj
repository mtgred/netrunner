(ns game-test.cards.programs.harbinger
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest harbinger
  ;; Harbinger
  (testing "install facedown when Blacklist installed"
    (do-game
      (new-game {:corp {:deck ["Blacklist"]}
                 :runner {:deck ["Harbinger"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Harbinger")
      (core/trash state :runner (-> (get-runner) :rig :program first))
      (is (zero? (count (:discard (get-runner)))) "Harbinger not in heap")
      (is (-> (get-runner) :rig :facedown first :facedown) "Harbinger installed facedown"))))
