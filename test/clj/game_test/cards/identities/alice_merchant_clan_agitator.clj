(ns game-test.cards.identities.alice-merchant-clan-agitator
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest alice-merchant-clan-agitator
  ;; Alice Merchant
  (do-game
    (new-game {:runner {:id "Alice Merchant: Clan Agitator"
                        :deck ["Security Testing"]}})
    ; (trash-from-hand state :corp "Hostile Takeover")
    (take-credits state :corp)
    (play-from-hand state :runner "Security Testing")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "Archives")
    (run-empty-server state "Archives")
    (click-prompt state :runner "Alice Merchant: Clan Agitator")
    (click-prompt state :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (is (= 1 (-> (get-corp) :discard count)) "Alice ability should trash 1 card from HQ")
    (is (-> (get-corp) :discard first :seen not) "Discarded card should be facedown when access is replaced")))
