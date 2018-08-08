(ns game-test.cards.assets.long-term-investment
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest long-term-investment
  ;; Long-Term Investment
  (do-game
    (new-game {:corp {:deck ["Long-Term Investment"]}})
    (play-from-hand state :corp "Long-Term Investment" "New remote")
    (let [lti (get-content state :remote1 0)]
      (core/rez state :corp lti)
      (dotimes [i 4]
        (is (= (* i 2) (get-counters (refresh lti) :credit)) "Long-Term Investement should gain 2 credits at start of turn")
        (take-credits state :corp)
        (take-credits state :runner))
      (is (= 8 (get-counters (refresh lti) :credit)) "Long-Term Investment should have 8 credit after 4 turns")
      (let [credits (:credit (get-corp))]
        (card-ability state :corp (refresh lti) 0)
        (click-prompt state :corp "8")
        (is (= (+ credits 8) (:credit (get-corp))) "Corp should gain 8 credits from Long-Term Investment ability")))))
