(ns game-test.cards.agendas.hades-fragment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hades-fragment
  ;; Hades Fragment
  (do-game
    (new-game {:corp {:deck ["Hades Fragment" (qty "Hedge Fund" 2)]}})
    (starting-hand state :corp ["Hades Fragment"])
    (play-and-score state "Hades Fragment")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 1 (count (:hand (get-corp)))) "Corp should have no opportunity to use Hades Shard")
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (take-credits state :corp)
    (take-credits state :runner)
    (let [hf-scored (get-scored state :corp 0)]
      (card-ability state :corp hf-scored 0)
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (= 2 (count (:deck (get-corp)))) "R&D should have 2 cards in it after Hades Fragment use"))))
