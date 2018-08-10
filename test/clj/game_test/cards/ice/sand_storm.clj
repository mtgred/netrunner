(ns game-test.cards.ice.sand-storm
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sand-storm
  ;; Sand Storm should not end the run if protecting an otherwise empty/naked server
  (do-game
    (new-game {:corp {:deck ["Sand Storm" "PAD Campaign"]}})
    (play-from-hand state :corp "Sand Storm" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (run-on state "Server 1")
    (let [sand-storm (get-ice state :remote1 0)]
      (core/rez state :corp sand-storm)
      (card-subroutine state :corp sand-storm 0)
      (click-prompt state :corp "Server 2")
      (is (= (first (get-in @state [:run :server])) :remote2) "Is running on server 2"))))
