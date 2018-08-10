(ns game-test.cards.assets.cpc-generator
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cpc-generator
  ;; CPC Generator
  (do-game
    (new-game {:corp {:deck ["CPC Generator"]}})
    (play-from-hand state :corp "CPC Generator" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (let [credits (:credit (get-corp))]
      (core/click-credit state :runner nil)
      (is (= 1 (- (:credit (get-corp)) credits)) "Should gain one from CPC Generator"))
    (let [credits (:credit (get-corp))]
      (core/click-credit state :runner nil)
      (is (zero? (- (:credit (get-corp)) credits)) "Shouldn't gain another credit from CPC Generator"))))
