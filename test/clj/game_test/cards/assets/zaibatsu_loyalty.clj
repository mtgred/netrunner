(ns game-test.cards.assets.zaibatsu-loyalty
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest zaibatsu-loyalty
  ;; Zaibatsu Loyalty
  (do-game
    (new-game {:corp {:deck ["Zaibatsu Loyalty" "Ice Wall"]}
               :runner {:deck ["Lemuria Codecracker"]}})
    (core/gain state :corp :click 10 :click 10)
    (play-from-hand state :corp "Zaibatsu Loyalty" "New remote")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 10 :click 10)
    (play-from-hand state :runner "Lemuria Codecracker")
    (let [code (get-hardware state 0)
          iw (get-ice state :remote2 0)
          zai (get-content state :remote1 0)]
      (run-empty-server state "HQ")
      (card-ability state :runner code 0)
      (click-card state :runner (refresh iw))
      (is (some? (-> (get-corp) :prompt first)) "Corp should get the option to rez Zaibatsu Loyalty before expose")
      (click-prompt state :corp "Yes")
      (is (:rezzed (refresh zai)) "Zaibatsu Loyalty should be rezzed")
      (let [credits (:credit (get-corp))]
        (card-ability state :corp zai 0)
        (is (= (- credits 1) (:credit (get-corp))) "Corp should lose 1 credit for stopping the expose")
        (click-prompt state :corp "Done"))
      (card-ability state :runner code 0)
      (click-card state :runner (refresh iw))
      (is (some? (-> (get-corp) :prompt first)) "Corp should be prompted to prevent")
      (is (= 0 (-> (get-corp) :discard count)) "No trashed cards")
      (card-ability state :corp zai 1)
      (is (= 1 (-> (get-corp) :discard count)) "Zaibatsu Loyalty should be in discard after using ability"))))
