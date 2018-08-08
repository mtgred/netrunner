(ns game-test.cards.agendas.remote-enforcement
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest remote-enforcement
  ;; Remote Enforcement - Search R&D for a piece of ice and install it on a remote at no rez cost
  (do-game
    (new-game {:corp {:deck [(qty "Remote Enforcement" 2)
                             "Archer"
                             "Chiyashi"]}
               :runner {:id "Reina Roja: Freedom Fighter"}})
    (starting-hand state :corp ["Remote Enforcement" "Remote Enforcement"])
    (is (= 2 (count (:deck (get-corp)))))
    (play-and-score state "Remote Enforcement")
    (let [N (:credit (get-corp))]
      (click-prompt state :corp "Yes")
      (click-prompt state :corp (find-card "Chiyashi" (:deck (get-corp))))
      (click-prompt state :corp "New remote")
      (is (core/rezzed? (get-ice state :remote2 0)) "Chiyashi was installed rezzed")
      (is (= N (:credit (get-corp))) "Rezzing Chiyashi was free"))
    (play-and-score state "Remote Enforcement")
    (let [N (:credit (get-corp))]
      (click-prompt state :corp "Yes")
      (click-prompt state :corp (find-card "Archer" (:deck (get-corp))))
      (click-prompt state :corp "Server 2")
      (is (= (dec N) (:credit (get-corp))) "Installing Archer cost a credit")
      (is (not-empty (:prompt (get-corp))) "Corp prompted to forfeit an agenda for Archer")
      (is (= (dec N) (:credit (get-corp))) "Rezzing Archer didn't cost any credits"))))
