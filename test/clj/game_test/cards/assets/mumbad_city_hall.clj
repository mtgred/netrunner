(ns game-test.cards.assets.mumbad-city-hall
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mumbad-city-hall
  ;; Mumbad City Hall
  (do-game
    (new-game {:corp {:deck ["Mumbad City Hall"
                             "PAD Factory"
                             "Salem's Hospitality"]}})
    (core/gain state :corp :click 3 :credit 100)
    (starting-hand state :corp ["Mumbad City Hall"])
    (play-from-hand state :corp "Mumbad City Hall" "New remote")
    (let [mumbad (get-content state :remote1 0)]
      (core/rez state :corp mumbad)
      (card-ability state :corp mumbad 0)
      (click-prompt state :corp (find-card "PAD Factory" (:deck (get-corp))))
      (click-prompt state :corp "New remote")
      (is (= "PAD Factory" (:title (get-content state :remote2 0))))
      (card-ability state :corp mumbad 0)
      (click-prompt state :corp (find-card "Salem's Hospitality" (:deck (get-corp))))
      (click-prompt state :corp "Sure Gamble")
      (is (= 3 (-> (get-runner) :discard count)) "Runner should have discarded all cards from Salem's Hospitality"))))
