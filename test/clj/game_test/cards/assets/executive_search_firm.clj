(ns game-test.cards.assets.executive-search-firm
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest executive-search-firm
  ;; Executive Search Firm
  (do-game
    (new-game {:corp {:deck ["Executive Search Firm" "Elizabeth Mills"
                             "Midori" "Shannon Claire"]}})
    (starting-hand state :corp ["Executive Search Firm"])
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Executive Search Firm" "New remote")
    (doseq [card ["Elizabeth Mills" "Midori" "Shannon Claire"]]
      (let [esf (get-content state :remote1 0)
            number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
        (card-ability state :corp esf 0)
        (click-prompt state :corp (find-card card (:deck (get-corp))))
        (is (= card (-> (get-corp) :hand first :title)) (str card " should be in hand"))
        (core/move state :corp (find-card card (:hand (get-corp))) :deck)
        (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Should be shuffled")))))
