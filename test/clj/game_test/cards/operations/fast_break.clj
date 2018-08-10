(ns game-test.cards.operations.fast-break
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest fast-break
  ;; Fast Break
  (do-game
    (new-game {:corp {:deck ["Fast Break" "Hostile Takeover" "Keegan Lane" "Haas Arcology AI"
                             "Research Station" (qty "Ice Wall" 10)]}
               :runner {:deck [(qty "Fan Site" 3)]}})
    (starting-hand state :corp ["Fast Break" "Hostile Takeover" "Keegan Lane"
                                "Haas Arcology AI" "Research Station"])
    (take-credits state :corp)
    (dotimes [_ 3]
      (play-from-hand state :runner "Fan Site"))
    (take-credits state :runner)
    (play-and-score state "Hostile Takeover")
    (is (= 3 (count (get-scored state :runner))) "Runner should have 3 agendas in score area")
    (play-from-hand state :corp "Fast Break")
    (let [hand (-> (get-corp) :hand count)
          credits (:credit (get-corp))]
      (click-prompt state :corp "3")
      (is (= (+ hand 3) (-> (get-corp) :hand count)) "Corp should draw 3 cards from Fast Break")
      (click-prompt state :corp "New remote")
      (click-card state :corp (find-card "Keegan Lane" (:hand (get-corp))))
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (is (= (dec credits) (:credit (get-corp))) "Corp should pay 1 credit to install second Ice Wall"))
    (core/move state :corp (find-card "Fast Break" (:discard (get-corp))) :hand)
    (play-from-hand state :corp "Fast Break")
    (let [hand (-> (get-corp) :hand count)
          credits (:credit (get-corp))]
      (click-prompt state :corp "0")
      (is (= hand (-> (get-corp) :hand count)) "Corp should draw no cards as they're allowed to draw no cards")
      (is (some #{"Server 2"} (:choices (prompt-map :corp))) "Corp should be able to choose existing remotes")
      (click-prompt state :corp "Server 2")
      (click-card state :corp (find-card "Haas Arcology AI" (:hand (get-corp))))
      (click-card state :corp (find-card "Research Station" (:hand (get-corp))))
      (is (= 2 (count (get-content state :remote2))) "Corp can't choose Research Station to install in a remote")
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
      (click-prompt state :corp "Done")
      (is (= (- credits 2) (:credit (get-corp))) "Corp should pay 2 credits to install third Ice Wall")
      (is (empty? (:prompt (get-corp))) "Corp should be able to stop installing early"))))
