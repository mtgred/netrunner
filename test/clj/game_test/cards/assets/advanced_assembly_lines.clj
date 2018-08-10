(ns game-test.cards.assets.advanced-assembly-lines
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest advanced-assembly-lines
  ;; Advanced Assembly Lines
  (do-game
    (new-game {:corp {:deck ["Advanced Assembly Lines"
                             "PAD Campaign"]}})
    (play-from-hand state :corp "Advanced Assembly Lines" "New remote")
    (let [aal (get-content state :remote1 0)
          credits (:credit (get-corp))
          hq (count (:hand (get-corp)))]
      (core/rez state :corp aal)
      (is (= (+ credits 2) (:credit (get-corp))) "Spend 1 gain 3")
      (card-ability state :corp aal 0)
      (click-card state :corp (find-card "PAD Campaign" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (is (= (- hq 1) (count (:hand (get-corp)))) "Installed 1 card, hq is empty"))))
