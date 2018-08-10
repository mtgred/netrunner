(ns game-test.cards.assets.grndl-refinery
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest grndl-refinery
  ;; GRNDL Refinery
  (do-game
    (new-game {:corp {:deck ["GRNDL Refinery"]}})
    (core/gain state :corp :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :corp "GRNDL Refinery" "New remote")
      (let [grndl (get-content state (keyword (str "remote" (inc i))) 0)
            credits (- (:credit (get-corp)) i)]
        (when (pos? i)
          (advance state (refresh grndl) i)
          (is (= i (get-counters (refresh grndl) :advancement)) (str "GRNDL Refinery should have " i " advancement counters on it")))
        (card-ability state :corp (refresh grndl) 0)
        (is (= (+ credits (* i 4)) (:credit (get-corp))) (str "Corp should gain " (* i 4) " credits"))
        (is (= 1 (-> (get-corp) :discard count)) "Archives should have 1 card in it")
        (is (= "GRNDL Refinery" (-> (get-corp) :discard first :title)) "Only card in Archives should be GRNDL Refinery")
        (core/move state :corp (find-card "GRNDL Refinery" (:discard (get-corp))) :hand)))))
