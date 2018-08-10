(ns game-test.cards.assets.zealous-judge
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest zealous-judge
  ;; Zealous Judge
  (do-game
    (new-game {:corp {:deck ["Zealous Judge"]}})
    (play-from-hand state :corp "Zealous Judge" "New remote")
    (let [judge (get-content state :remote1 0)]
      (core/rez state :corp judge)
      (is (not (:rezzed (refresh judge))) "Zealous Judge can't be rezzed until Runner is tagged")
      (core/gain state :runner :tag 1)
      (core/rez state :corp judge)
      (is (:rezzed (refresh judge)) "Zealous Judge can be rezzed while the Runner is tagged")
      (card-ability state :corp judge 0)
      (is (= 2 (:tag (get-runner))) "Runner should gain a tag from Zealous Judge's ability"))))
