(ns game-test.cards.assets.worlds-plaza
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest worlds-plaza
  ;; Worlds Plaza
  (do-game
    (new-game {:corp {:deck ["Worlds Plaza"
                             "Personalized Portal"
                             "Dedicated Response Team"
                             "Honeyfarm"]}})
    (core/gain state :corp :credit 10 :click 10)
    (play-from-hand state :corp "Worlds Plaza" "New remote")
    (let [plaza (get-content state :remote1 0)]
      (core/rez state :corp plaza)
      (card-ability state :corp plaza 0)
      (let [credits (:credit (get-corp))]
        (card-ability state :corp plaza 0)
        (click-card state :corp (find-card "Personalized Portal" (:hand (get-corp))))
        (is (= (- credits 1) (:credit (get-corp))) "Corp should only spend 1 credit to rez Personalized Portal"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp plaza 0)
        (click-card state :corp (find-card "Dedicated Response Team" (:hand (get-corp))))
        (is (= credits (:credit (get-corp))) "Corp should spend 0 credit to rez Dedicated Response Team"))
      (let [credits (:credit (get-corp))]
        (card-ability state :corp plaza 0)
        (click-card state :corp (find-card "Honeyfarm" (:hand (get-corp))))
        (is (= credits (:credit (get-corp))) "Corp should spend 0 credit to rez Honeyfarm")))))
