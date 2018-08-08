(ns game-test.cards.upgrades.ben-musashi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ben-musashi
  ;; Ben Musashi
  (testing "Basic test - pay 2 net damage to steal from this server"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "House of Knives"]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [bm (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 2 net damage")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner did not pay 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-card state :runner bm)
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "on R&D access"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "House of Knives"]}})
      (starting-hand state :corp ["Ben Musashi"])
      (play-from-hand state :corp "Ben Musashi" "R&D")
      (take-credits state :corp)
      (let [bm (get-content state :rd 0)]
        (core/rez state :corp bm)
        (run-empty-server state "R&D")
        ;; runner now chooses which to access.
        (click-prompt state :runner "Card from deck")
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay 2 net damage")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner did not pay 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "Ben Musashi")
        (click-prompt state :runner "No action")
        (run-empty-server state "R&D")
        (click-prompt state :runner "Card from deck")
        (click-prompt state :runner "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "pay even when trashed"
    (do-game
      (new-game {:corp {:deck [(qty "Ben Musashi" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [bm (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner bm)
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; pay to trash
        (click-card state :runner hok)
        ;; should now have prompt to pay 2 net for HoK
        (click-prompt state :runner "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))
  (testing "Check runner chooses order of payment"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "Obokata Protocol"]}
                 :runner {:deck [(qty "Sure Gamble" 6)]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "Obokata Protocol" "Server 1")
      (take-credits state :corp)
      (let [bm (get-content state :remote1 0)
            op (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner op)
        ;; prompt should be asking for the net damage costs
        (is (= "Obokata Protocol" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay steal costs")
        (click-prompt state :runner "Pay to steal")
        (click-prompt state :runner "2 net damage")
        (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "4 net damage")
        (is (= 5 (count (:discard (get-runner)))) "Runner took 4 net damage")
        (is (= 1 (count (:scored (get-runner)))) "Scored agenda"))))
  (testing "Check Fetal AI can be stolen, #2586"
    (do-game
      (new-game {:corp {:deck ["Ben Musashi" "Fetal AI"]}
                 :runner {:deck [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Ben Musashi" "New remote")
      (play-from-hand state :corp "Fetal AI" "Server 1")
      (take-credits state :corp)
      (let [bm (get-content state :remote1 0)
            fai (get-content state :remote1 1)]
        (core/rez state :corp bm)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner fai)
        ;; prompt should be asking for the net damage costs
        (is (= "Fetal AI" (:title (:card (first (:prompt (get-runner))))))
            "Prompt to pay steal costs")
        (click-prompt state :runner "Pay to steal")
        (click-prompt state :runner "2 [Credits]")
        (is (= 3 (:credit (get-runner))) "Runner paid 2 credits")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "2 net damage")
        (is (= 4 (count (:discard (get-runner)))) "Runner took 4 net damage - 2 from Fetal, 2 from Ben")
        (is (= 1 (count (:scored (get-runner)))) "Scored agenda")))))
