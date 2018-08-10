(ns game-test.cards.assets.raman-rai
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest raman-rai
  ;; Raman Rai
  (do-game
    (new-game {:corp {:deck ["Raman Rai" "Ice Wall" "Fire Wall"]}})
    (play-from-hand state :corp "Raman Rai" "New remote")
    (let [raman (get-content state :remote1 0)]
      (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
      (trash-from-hand state :corp "Fire Wall")
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp raman 0)
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-card state :corp (find-card "Fire Wall" (:discard (get-corp))))
      (is (= "Fire Wall" (-> (get-corp) :hand first :title)))
      (is (= "Ice Wall" (-> (get-corp) :discard first :title))))))
