(ns game-test.cards.assets.estelle-moon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest estelle-moon
  ;; Estelle Moon
  (letfn [(estelle-test [number]
            (do-game
              (new-game {:corp {:deck ["Estelle Moon" (qty "Encryption Protocol" 20)]}})
              (starting-hand state :corp (repeat 9 "Encryption Protocol"))
              (core/move state :corp (find-card "Estelle Moon" (:deck (get-corp))) :hand)
              (play-from-hand state :corp "Estelle Moon" "New remote")
              (let [em (get-content state :remote1 0)]
                (core/rez state :corp (refresh em))
                (core/gain state :corp :click 10)
                (dotimes [_ number]
                  (play-from-hand state :corp "Encryption Protocol" "New remote"))
                (let [credits (:credit (get-corp))
                      hand (count (:hand (get-corp)))]
                  (card-ability state :corp (refresh em) 0)
                  (is (= (* 2 number) (- (:credit (get-corp)) credits)) (str "Should gain " (* 2 number) " credits"))
                  (is (= number (- (count (:hand (get-corp))) hand)) (str "Should draw " number " cards"))
                  (is (= 1 (-> (get-corp) :discard count)) "Estelle Moon should be trashed")))))]
    (doall (map estelle-test (range 10)))))
