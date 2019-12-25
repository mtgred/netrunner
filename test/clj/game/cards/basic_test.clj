(ns game.cards.basic-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [card-defs]]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest ^:test-refresh/focus runner-basic-actions
  (testing "Draw card"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}})
      (core/click-draw state :corp nil)
      (println (clojure.string/join "\n" (map :text (:log @state))))
      )))
