(ns game-test.cards.assets.marilyn-campaign
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest marilyn-campaign
  ;; Marilyn Campaign
  (do-game
    (new-game {:corp {:deck ["Marilyn Campaign"]}})
    (play-from-hand state :corp "Marilyn Campaign" "New remote")
    (let [marilyn (get-content state :remote1 0)]
      (core/rez state :corp marilyn)
      (is (= 8 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should start with 8 credits")
      (is (zero? (-> (get-corp) :deck count)) "R&D should be empty")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 6 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 4 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (zero? (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (click-prompt state :corp "Yes")
      (is (= 1 (-> (get-corp) :hand count)) "HQ should have 1 card in it, after mandatory draw")
      (is (= "Marilyn Campaign" (-> (get-corp) :hand first :title)) "Marilyn Campaign should be in HQ, after mandatory draw"))))
