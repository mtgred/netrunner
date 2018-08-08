(ns game-test.cards.identities.sso-industries-fueling-innovation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sso-industries-fueling-innovation
  ;; SSO Industries: Fueling Innovation - add advancement tokens on ice for faceup agendas
  (do-game
    (new-game {:corp {:id "SSO Industries: Fueling Innovation"
                      :deck [(qty "Hortum" 2) (qty "Oaktown Renovation" 2) "Braintrust"]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (is (empty? (:prompt (get-corp))) "Not prompted when no faceup agenda available")
    (take-credits state :runner)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (take-credits state :corp)
    (is (empty? (:prompt (get-corp))) "Not prompted when no ice available")
    (take-credits state :runner)
    (play-from-hand state :corp "Hortum" "HQ")
    (play-from-hand state :corp "Hortum" "R&D")
    (let [h0 (get-ice state :hq 0)
          h1 (get-ice state :rd 0)]
      (is (zero? (get-counters (refresh h0) :advancement)) "Starts with 0 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Starts with 0 tokens")
      (take-credits state :corp)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh h0))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Gains 2 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Stays at 0 tokens")
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (take-credits state :corp)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh h1))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Stays at 2 tokens")
      (is (= 4 (get-counters (refresh h1) :advancement)) "Gains 4 tokens")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (empty? (:prompt (get-corp))) "Not prompted when all ice advanced"))))
