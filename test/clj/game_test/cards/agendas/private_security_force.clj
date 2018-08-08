(ns game-test.cards.agendas.private-security-force
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest private-security-force
  ;; Private Security Force
  (do-game
    (new-game {:corp {:deck [(qty "Private Security Force" 10)]}})
    (core/gain state :runner :tag 1)
    (play-and-score state "Private Security Force")
    (let [psf-scored (get-scored state :corp 0)]
      (card-ability state :corp psf-scored 0)
      (is (= 1 (count (:discard (get-runner)))))
      (take-credits state :runner)
      (dotimes [n 3]
        (card-ability state :corp psf-scored 0))
      (is (= 3 (count (:discard (get-runner)))))
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
