(ns game-test.cards.assets.illegal-arms-factory
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest illegal-arms-factory
  ;; Illegal Arms Factory; draw a card, gain a credit, bad pub when trashed while rezzed
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"
                             "Beanstalk Royalties"
                             "IPO"
                             (qty "Illegal Arms Factory" 3)]}})
    (core/gain state :runner :credit 20)
    (core/move state :corp (find-card "IPO" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Beanstalk Royalties" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Illegal Arms Factory" "New remote")
    (play-from-hand state :corp "Illegal Arms Factory" "New remote")
    (let [iaf (get-content state :remote2 0)]
      (core/rez state :corp iaf)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (zero? (:bad-publicity (get-corp))) "Took no bad pub on unrezzed trash")
      (take-credits state :runner)
      (is (= 3 (count (:hand (get-corp)))) "Drew a card from IAF + mandatory")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit from IAF")
      (take-credits state :corp)
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (= 1 (:bad-publicity (get-corp))) "Took a bad pub on rezzed trash"))))
