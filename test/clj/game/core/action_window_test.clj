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
    ; (println (:gp @state))
    (core/gain state :corp :click 1)
    (is (= ["[Click]: Gain 1 [Credits]"
            "[Click]: Draw 1 card"
            "[Click][Click][Click]: Purge virus counters"]
           (map :title (prompt-buttons :corp))))
    (click-prompt state :corp "[Click][Click][Click]: Purge virus counters")
    (is (= 1 (:click (get-corp))))
    (is (= ["[Click]: Gain 1 [Credits]"
            "[Click]: Draw 1 card"]
           (map :title (prompt-buttons :corp))))
    (click-prompt state :corp "[Click]: Gain 1 [Credits]")
    (is (some? (get-current-step state)))
    (is (= [] (map :title (prompt-buttons :corp))))))
