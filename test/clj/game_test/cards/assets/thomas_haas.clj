(ns game-test.cards.assets.thomas-haas
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest thomas-haas
  ;; Thomas Haas
  (letfn [(haas-test [number]
            (do-game
              (new-game {:corp {:deck ["Thomas Haas"]}})
              (core/gain state :corp :credit 10 :click 10)
              (play-from-hand state :corp "Thomas Haas" "New remote")
              (let [haas (get-content state :remote1 0)]
                (core/rez state :corp haas)
                (advance state (refresh haas) number)
                (core/lose state :corp :credit (:credit (get-corp)))
                (is (zero? (-> (get-corp) :discard count)) "Corp should start with 0 cards in Archives")
                (is (zero? (:credit (get-corp))) "Corp should fire ability with 0 credits")
                (is (= number (get-counters (refresh haas) :advancement))
                    (str "Thomas Haas should have " number " advancement tokens"))
                (card-ability state :corp (refresh haas) 0)
                (is (= (* 2 number) (:credit (get-corp)))
                    (str "Corp should gain " (* 2 number) " credits from Thomas Haas' ability"))
                (is (= 1 (-> (get-corp) :discard count)) "Thomas Haas should be in Archives after ability"))))]
    (doall (map haas-test [1 2 3 4 5]))))
