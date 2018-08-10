(ns game-test.cards.assets.museum-of-history
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest museum-of-history
  ;; Museum of History
  (do-game
    (new-game {:corp {:deck ["Museum of History" "Beanstalk Royalties"
                             (qty "Ice Wall" 10)]}})
    (starting-hand state :corp ["Beanstalk Royalties" "Museum of History"])
    (play-from-hand state :corp "Beanstalk Royalties")
    (play-from-hand state :corp "Museum of History" "New remote")
    (let [museum (get-content state :remote1 0)
          number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))]
      (core/rez state :corp museum)
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp museum 0)
      (click-card state :corp (find-card "Beanstalk Royalties" (:discard (get-corp))))
      (is (< number-of-shuffles (count (core/turn-events state :corp :corp-shuffle-deck))) "Corp should shuffle deck")
      (is (zero? (-> (get-corp) :discard count)) "Archives should be empty after shuffling Beanstalk into R&D"))))
