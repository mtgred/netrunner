(ns game-test.cards.assets.whampoa-reclamation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest whampoa-reclamation
  ;; Whampoa Reclamation: Enable trashing a card from HQ to place a card in Archives on the bottom of R&D
  (do-game
    (new-game {:corp {:deck [(qty "Whampoa Reclamation" 3)
                             (qty "Global Food Initiative" 3)]}})
    (play-from-hand state :corp "Whampoa Reclamation" "New remote")
    (core/trash state :corp (find-card "Whampoa Reclamation" (:hand (get-corp))))
    (let [wr (get-content state :remote1 0)
          gfi (find-card "Global Food Initiative" (:hand (get-corp)))]
      (core/rez state :corp wr)
      (take-credits state :corp)
      (card-ability state :corp wr 0)
      (click-card state :corp gfi)
      (click-card state :corp (find-card "Global Food Initiative" (:discard (get-corp))))
      (is (= 1 (count (:discard (get-corp)))) "Only card in discard placed in bottom of R&D")
      (is (= "Global Food Initiative" (-> (get-corp) :deck last :title)) "GFI last card in deck"))))
