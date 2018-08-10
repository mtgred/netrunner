(ns game-test.cards.identities.new-angeles-sol-your-news
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest new-angeles-sol-your-news
  ;; New Angeles Sol - interaction with runner stealing agendas
  (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck [(qty "Paywall Implementation" 2) "Breaking News"]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Paywall Implementation")
    (take-credits state :corp)
    (is (= 6 (:credit (get-corp))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-corp))) "Corp gained 1cr from successful run")
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-corp))) "Paywall trashed before Sol triggers")
    (click-card state :corp (find-card "Paywall Implementation" (:hand (get-corp))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall back in play")))
