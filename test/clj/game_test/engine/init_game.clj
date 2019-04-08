(ns game-test.engine.init-game
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest default-identity
  (do-game
    (new-game)
    (is (= "The Masque: Cyber General" (get-in @state [:runner :identity :title])))
    (is (= "The Shadow: Pulling the String" (get-in @state [:corp :identity :title])))))
