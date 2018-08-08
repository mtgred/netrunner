(ns game-test.cards.assets.mumbad-construction-co
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mumbad-construction-co
  ;; Mumbad Construction Co.
  (do-game
    (new-game {:corp {:deck ["Mumbad Construction Co."
                             "Oaktown Renovation"]}})
    (play-from-hand state :corp "Mumbad Construction Co." "New remote")
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [mcc (get-content state :remote1 0)
          oak (get-content state :remote2 0)]
      (core/rez state :corp mcc)
      (is (zero? (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should start with 0 counters")
      (is (zero? (get-counters (refresh oak) :advancement)) "Oaktown Renovation should start with 0 counters")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should gain 1 counter at start of turn")
      (let [credits (:credit (get-corp))]
        (card-ability state :corp mcc 0)
        (click-card state :corp (refresh oak))
        (is (zero? (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should lose 1 counter when using ability")
        (is (= 1 (get-counters (refresh oak) :advancement)) "Oaktown Renovation should gain 1 counter from MCC ability")
        (is (= (- credits 2) (:credit (get-corp))) "Mumbad Construction Co ability should cost 2 [Credits]")))))
