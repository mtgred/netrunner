(ns game-test.cards.assets.bioroid-work-crew
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bioroid-work-crew
  ;; Bioroid Work Crew
  (letfn [(bwc-test [card]
            (do-game
              (new-game {:corp {:deck ["Bioroid Work Crew" card]}})
              (play-from-hand state :corp "Bioroid Work Crew" "New remote")
              (let [bwc (get-content state :remote1 0)]
                (core/rez state :corp bwc)
                (card-ability state :corp bwc 0)
                (click-card state :corp card)
                (if (= "Research Station" card)
                  (click-prompt state :corp "HQ")
                  (click-prompt state :corp "New remote"))
                (is (zero? (count (:hand (get-corp)))))
                (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now"))))]
    (doall (map bwc-test
                ["Hostile Takeover"
                 "Dedicated Response Team"
                 "Builder"
                 "Research Station"]))))
