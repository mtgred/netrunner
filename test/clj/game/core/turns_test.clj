(ns game.core.turns-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.core.turns :as sut]
            [game.core.pipeline :refer :all]
            [clojure.test :refer :all]))

(deftest turns-test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Melange Mining Corp."]
                      :credits 10}})
    )
  )
