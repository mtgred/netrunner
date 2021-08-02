(ns game.core.action-window-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.core.action-window :as sut]
            [game.core.pipeline :refer :all]
            [clojure.test :refer :all]))

(deftest generate-prompts-test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Melange Mining Corp."]
                      :credits 10}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (rez state :corp (get-content state :remote1 0))
    (sut/start-action-phase state)
    (continue-gp! state)
    (is (= [
            "[Click]: Gain 1 [Credits]"
            "[Click]: Draw 1 card"
            "[Click][Click][Click]: Purge virus counters"
            ]
           (map :title (prompt-buttons :corp))))
    (click-prompt state :corp "[Click][Click][Click]: Purge virus counters")
    (is (= 1 (:click (get-corp))))
    (is (= [
            "[Click]: Gain 1 [Credits]"
            "[Click]: Draw 1 card"
            ]
           (map :title (prompt-buttons :corp))))
    (changes-val-macro
        1 (:credit (get-corp))
        "msg"
        (click-prompt state :corp "[Click]: Gain 1 [Credits]"))
    (is (nil? (get-current-step state)))
    (is (empty? (:prompt (get-corp))))
    ))
