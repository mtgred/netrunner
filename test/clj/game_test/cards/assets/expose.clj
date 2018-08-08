(ns game-test.cards.assets.expose
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest expose
  ;; Exposé
  (do-game
    (new-game {:corp {:deck ["Exposé"]}})
    (core/gain state :corp :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :corp "Exposé" "New remote")
      (let [expose (get-content state (keyword (str "remote" (inc i))) 0)]
        (core/rez state :corp (refresh expose))
        (is (zero? (:bad-publicity (get-corp))) "Corp should have 0 bad publicity to start with")
        (when (pos? i)
          (core/gain-bad-publicity state :corp i)
          (is (= i (:bad-publicity (get-corp))) (str "Corp should gain " i " bad publicity"))
          (advance state (refresh expose) i))
        (card-ability state :corp (refresh expose) 0)
        (is (zero? (:bad-publicity (get-corp))) "Corp should have 0 bad publicity after using Exposé's ability")
        (is (= 1 (-> (get-corp) :discard count)) "Archives should have 1 card in it")
        (is (= "Exposé" (-> (get-corp) :discard first :title)) "Only card in Archives should be Exposé")
        (core/move state :corp (find-card "Exposé" (:discard (get-corp))) :hand)))))
