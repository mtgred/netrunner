(ns game-test.cards.events.falsified-credentials
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest falsified-credentials
  ;; Falsified Credentials - Expose card in remote
  ;; server and correctly guess its type to gain 5 creds
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Eve Campaign" 2)
                               (qty "Product Placement" 2)
                               "Project Atlas"]}
                 :runner {:deck [(qty "Falsified Credentials" 3)]}})
      (core/gain state :corp :click 2)
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Product Placement" "HQ")
      (play-from-hand state :corp "Product Placement" "Server 3")
      (take-credits state :corp)
      (let [eve1 (get-content state :remote1 0)
            eve2 (get-content state :remote2 0)
            atl (get-content state :remote3 0)
            pp1 (get-content state :hq 0)
            pp2 (get-content state :remote3 1)]
        (core/rez state :corp eve1)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Asset")
        (click-card state :runner (refresh eve1))
        (is (= 4 (:credit (get-runner)))
            "Rezzed cards can't be targeted")
        (click-card state :runner eve2)
        (is (= 3 (:click (get-runner))) "Spent 1 click")
        (is (= 9 (:credit (get-runner))) "Gained 5 creds for guessing asset correctly")
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Upgrade")
        (click-card state :runner pp1)
        (is (= 8 (:credit (get-runner))) "Can't target cards in centrals")
        (click-card state :runner pp2)
        (is (= 13 (:credit (get-runner)))
            "Gained 5 creds for guessing upgrade correctly, even if server contains non-upgrade as well")
        (core/rez state :corp pp2)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (is (= 17 (:credit (get-runner)))
            "Gained 5 credits for guessing agenda correctly, even with rezzed card in server"))))
  (testing "vs Zaibatsu Loyalty. If Falsified Credentials fails to expose, it grants no credits."
    (do-game
      (new-game {:corp {:deck ["Zaibatsu Loyalty" "Project Atlas"]}
                 :runner {:deck [(qty "Falsified Credentials" 2)]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Zaibatsu Loyalty" "New remote")
      (take-credits state :corp)
      (let [atl (get-content state :remote1 0)
            zaibatsu (get-content state :remote2 0)]
        (core/rez state :corp zaibatsu)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (click-prompt state :corp "Done")
        (is (= 9 (:credit (get-runner))) "An unprevented expose gets credits")
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (card-ability state :corp (refresh zaibatsu) 0) ; prevent the expose!
        (click-prompt state :corp "Done")
        (is (= 8 (:credit (get-runner))) "A prevented expose does not")))))
