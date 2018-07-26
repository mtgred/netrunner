(ns game-test.card-existence
  (:require [game.core :as core]
            [jinteki.cards :refer [all-cards]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards)

(deftest can-play-all-cards
  (doseq [[title card] @all-cards]
    (core/card-def card)))
