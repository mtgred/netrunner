(ns game-test.cards.identities.jemison-astronautics-sacrifice-audacity-success
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jemison-astronautics-sacrifice-audacity-success
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Enforcer 1.0" "Hostile Takeover" "Ice Wall" "Global Food Initiative"]}
                 :runner {:deck ["Data Dealer"]}})
      (play-from-hand state :corp "Enforcer 1.0" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (let [enf (get-ice state :hq 0)
            iwall (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Data Dealer")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (let [dd (get-resource state 0)]
          (card-ability state :runner dd 0)
          (click-card state :runner (get-in (get-runner) [:scored 0]))
          (is (empty? (:prompt (get-corp))) "No Jemison prompt for Runner forfeit")
          (take-credits state :runner)
          (play-from-hand state :corp "Global Food Initiative" "New remote")
          (score-agenda state :corp (get-content state :remote2 0))
          (core/rez state :corp enf)
          (click-card state :corp (get-in (get-corp) [:scored 0]))
          (click-card state :corp iwall)
          (is (= 4 (get-counters (refresh iwall) :advancement)) "Jemison placed 4 advancements")))))
  (testing "24/7 - Armed Intimidation combination"
    ;; Expected result: 24/7 causes Forfeit, Jemison places counters, AI triggers
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Armed Intimidation" "Hostile Takeover"
                               "24/7 News Cycle" "Ice Wall"]}})
      (play-and-score state "Hostile Takeover")
      (is (= 1 (:agenda-point (get-corp))) "Corp has 1 agenda points from Hostile Takeover")
      (is (= 12 (:credit (get-corp))) "Corp has 12 credits after scoring Hostile Takeover with play-score")
      (play-and-score state "Armed Intimidation")
      (click-prompt state :runner "Take 2 tags")
      (is (= 3 (:agenda-point (get-corp))) "Corp has 3 agenda points from HT + Armed Intimidation")
      (is (= 2 (:tag (get-runner))) "Runner took 2 tags from AI")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "24/7 News Cycle")
      (click-card state :corp (get-scored state :corp 0)) ; select HT to forfeit
      (let [ice-wall (get-ice state :hq 0)]
        (click-card state :corp ice-wall) ; The Jemison forfeit triggers
        (is (= 2 (get-counters (refresh ice-wall) :advancement)) "Ice Wall has 2 advancement counters from HT forfeit"))
      (click-card state :corp (get-scored state :corp 0)) ; select AI to trigger
      (click-prompt state :runner "Take 2 tags") ; First runner has prompt
      (is (= 4 (:tag (get-runner))) "Runner took 2 more tags from AI -- happens at the end of all the async completion"))))
