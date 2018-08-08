(ns game-test.cards.assets.tech-startup
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tech-startup
  ;; Tech Startup
  (do-game
    (new-game {:corp {:deck ["Tech Startup" "TechnoCo" (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Tech Startup"])
    (play-from-hand state :corp "Tech Startup" "New remote")
    (let [tech (get-content state :remote1 0)]
      (core/rez state :corp (refresh tech))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (zero? (-> (get-corp) :discard count)) "Corp should start with 0 cards in Archives")
      (card-ability state :corp tech 0)
      (click-prompt state :corp (find-card "TechnoCo" (:deck (get-corp))))
      (click-prompt state :corp "New remote")
      (is (= "TechnoCo" (:title (get-content state :remote2 0)))
          "TechnoCo should be installed in a new remote from Tech Startup's ability")
      (is (= 1 (-> (get-corp) :discard count)) "Tech Startup should now be in discard"))))
