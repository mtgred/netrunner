(ns game-test.cards.agendas.project-vitruvius
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest project-vitruvius
  ;; Project Vitruvius
  (do-game
    (new-game {:corp {:deck ["Project Vitruvius"
                             "Hedge Fund"]}})
    ;; Set up
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (is (= 1 (count (:discard (get-corp)))) "Corp should have 1 cards in hand")
    (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand")
    (core/gain state :corp :click 10 :credit 10)
    ;; Should gain 1 counter
    (play-from-hand state :corp "Project Vitruvius" "New remote")
    (let [vit (get-content state :remote1 0)]
      (advance state vit 4)
      (is (= 4 (get-counters (refresh vit) :advancement)) "Vitruvius should have 4 advancement tokens")
      (core/score state :corp {:card (refresh vit)}))
    (let [vit-scored (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 1 agenda counter")
      (card-ability state :corp vit-scored 0)
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (is (zero? (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 0 agenda counters")
      (is (= 1 (count (:hand (get-corp)))) "Corp should have 1 cards in hand"))))
