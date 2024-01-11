(ns game.core.board-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.test-framework :refer :all]))

(deftest all-installed
  (testing "corp cards"
    (do-game
      (new-game {:corp {:hand [;; Agendas
                               "Merger" "Hostile Takeover"
                               ;; Assets
                               "PAD Campaign" "Worlds Plaza" "NGO Front"
                               ;; Ice
                               "Ice Wall" "Vanilla"
                               ;; Upgrades
                               "Research Station" "Embolus"
                               ;; Hosted Operations
                               "MCA Informant" "Patch"
                               ;; Currents
                               "Death and Taxes"
                               ;; Removed from the game
                               "Game Changer"
                               ;; Operation during resolution
                               "Distract the Masses"
                               ;; Card left in hand
                               "Enigma"
                               ]
                        :deck ["Hedge Fund"]
                        :discard ["IPO"]
                        :credits 100}
                 :runner {:hand ["Film Critic"]}})
      (core/gain state :corp :click 100)
      ;; Agendas
      (play-and-score state "Hostile Takeover")
      (is (not (find-card "Hostile Takeover" (core/all-installed state :corp))))
      (play-from-hand state :corp "Merger" "New remote")
      (is (find-card "Merger" (core/all-installed state :corp)))
      ;; Assets
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (find-card "PAD Campaign" (core/all-installed state :corp)))
      (play-from-hand state :corp "Worlds Plaza" "New remote")
      (rez state :corp (get-content state :remote4 0))
      (is (find-card "Worlds Plaza" (core/all-installed state :corp)))
      (card-ability state :corp (get-content state :remote4 0) 0)
      (click-card state :corp "NGO Front")
      (is (find-card "NGO Front" (core/all-installed state :corp)))
      ;; Ice
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (find-card "Ice Wall" (core/all-installed state :corp)))
      (play-from-hand state :corp "Vanilla" "New remote")
      (is (find-card "Vanilla" (core/all-installed state :corp)))
      ;; Upgrades
      ;; Root
      (play-from-hand state :corp "Research Station" "HQ")
      (is (find-card "Research Station" (core/all-installed state :corp)))
      ;; Remotes
      (play-from-hand state :corp "Embolus" "New remote")
      (is (find-card "Embolus" (core/all-installed state :corp)))
      ;; Hosted Operations
      (rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Patch")
      (click-card state :corp "Ice Wall")
      (is (find-card "Patch" (core/all-installed state :corp)))
      ;; Currents
      (play-from-hand state :corp "Death and Taxes")
      (is (not (find-card "Death and Taxes" (core/all-installed state :corp))))
      ;; RFG
      (play-from-hand state :corp "Game Changer")
      (is (not (find-card "Game Changer" (core/all-installed state :corp))))
      ;; Operation in the play area
      (play-from-hand state :corp "Distract the Masses")
      (is (not (find-card "Distract the Masses" (core/all-installed state :corp))))
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Done")
      ;; Hand
      (is (not (find-card "Enigma" (core/all-installed state :corp))))
      ;; Deck
      (is (not (find-card "Hedge Fund" (core/all-installed state :corp))))
      ;; Discard
      (is (not (find-card "IPO" (core/all-installed state :corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (take-credits state :runner)
      (play-from-hand state :corp "MCA Informant")
      (click-card state :corp "Film Critic")
      ;; Hosted on runner cards
      (is (find-card "MCA Informant" (core/all-installed state :corp)))))
  (testing "runner cards"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Parasite"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Parasite")
      (click-card state :runner "Ice Wall")
      (is (find-card "Parasite" (core/all-installed state :runner))))))
