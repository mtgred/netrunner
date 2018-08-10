(ns game-test.cards.upgrades.bryan-stinson
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest bryan-stinson
  ;; Bryan Stinson - play a transaction from archives and remove from game. Ensure Currents are RFG and not trashed.
  (do-game
    (new-game {:corp {:deck ["Bryan Stinson" "Death and Taxes"
                             "Paywall Implementation" "Global Food Initiative"
                             "IPO"]}
               :runner {:deck ["Interdiction"]}})
    (trash-from-hand state :corp "Death and Taxes")
    (play-from-hand state :corp "Bryan Stinson" "New remote")
    (let [bs (get-content state :remote1 0)]
      (core/rez state :corp (refresh bs))
      (card-ability state :corp (refresh bs) 0)
      (click-prompt state :corp (find-card "Death and Taxes" (:discard (get-corp))))
      (is (find-card "Death and Taxes" (:current (get-corp))) "Death and Taxes is active Current")
      (take-credits state :corp)
      (play-from-hand state :runner "Interdiction")
      (is (find-card "Interdiction" (:current (get-runner))) "Interdiction is active Current")
      (is (find-card "Death and Taxes" (:rfg (get-corp))) "Death and Taxes removed from game")
      (is (not= "Death and Taxes" (:title (first (:discard (get-corp))))) "Death and Taxes not moved to trash")
      (take-credits state :runner)
      (core/lose state :runner :credit 3)
      (trash-from-hand state :corp "Paywall Implementation")
      (card-ability state :corp (refresh bs) 0)
      (click-prompt state :corp (find-card "Paywall Implementation" (:discard (get-corp))))
      (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall Implementation is active Current")
      (is (find-card "Interdiction" (:discard (get-runner))) "Interdiction is trashed")
      (trash-from-hand state :corp "IPO")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "Steal")
      (is (find-card "Paywall Implementation" (:rfg (get-corp))) "Paywall Implementation removed from game")
      (is (not= "Paywall Implementation" (:title (first (:discard (get-corp))))) "Paywall Implementation not moved to trash")
      (take-credits state :runner)
      (core/lose state :runner :credit 3)
      (card-ability state :corp (refresh bs) 0)
      (click-prompt state :corp (find-card "IPO" (:discard (get-corp))))
      (is (find-card "IPO" (:rfg (get-corp))) "IPO is removed from game"))))
