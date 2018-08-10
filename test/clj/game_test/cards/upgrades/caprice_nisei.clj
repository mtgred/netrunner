(ns game-test.cards.upgrades.caprice-nisei
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest caprice-nisei
  ;; Caprice Nisei - Psi game for ETR after runner passes last ice
  (do-game
    (new-game {:corp {:deck [(qty "Caprice Nisei" 3) (qty "Quandary" 3)]}})
    (play-from-hand state :corp "Caprice Nisei" "New remote")
    (take-credits state :corp)
    (let [caprice (get-content state :remote1 0)]
      ;; Check Caprice triggers properly on no ice (and rezzed)
      (core/rez state :corp caprice)
      (run-on state "Server 1")
      (is (prompt-is-card? state :corp caprice)
          "Caprice prompt even with no ice, once runner makes run")
      (is (prompt-is-card? state :runner caprice) "Runner has Caprice prompt")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (take-credits state :runner)
      (play-from-hand state :corp "Quandary" "Server 1")
      (play-from-hand state :corp "Quandary" "Server 1")
      (take-credits state :corp)
      ;; Check Caprice triggers properly on multiple ice
      (run-on state "Server 1")
      (run-continue state)
      (is (empty? (get-in @state [:corp :prompt])) "Caprice not trigger on first ice")
      (run-continue state) ; Caprice prompt after this
      (is (prompt-is-card? state :corp caprice)
          "Corp has Caprice prompt (triggered automatically as runner passed last ice)")
      (is (prompt-is-card? state :runner caprice) "Runner has Caprice prompt")
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (not (:run @state)) "Run ended by Caprice")
      (is (empty? (get-in @state [:corp :prompt])) "Caprice prompted cleared")
      ;; Check Caprice does not trigger on other servers
      (run-on state "HQ")
      (is (empty? (get-in @state [:corp :prompt])) "Caprice does not trigger on other servers"))))
