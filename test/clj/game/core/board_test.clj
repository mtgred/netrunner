(ns game.core.board-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest all-installed
  (testing "corp cards"
  (before-each [state (new-game {:corp {:hand [;; Agendas
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
                                 :runner {:hand ["Film Critic"]}})]
    (testing "Agendas"
      (do-game state
        (play-and-score state "Hostile Takeover")
        (is (not (find-card "Hostile Takeover" (core/all-installed state :corp))))
        (play-from-hand state :corp "Merger" "New remote")
        (is (find-card "Merger" (core/all-installed state :corp)))))
    (testing "Assets"
      (do-game state
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (is (find-card "PAD Campaign" (core/all-installed state :corp)))
        (play-from-hand state :corp "Worlds Plaza" "New remote")
        (rez state :corp (get-content state :remote2 0))
        (is (find-card "Worlds Plaza" (core/all-installed state :corp)))
        (card-ability state :corp (get-content state :remote2 0) 0)
        (click-card state :corp "NGO Front")
        (is (find-card "NGO Front" (core/all-installed state :corp)))))
    (testing "Ice"
      (do-game state
        (play-from-hand state :corp "Ice Wall" "HQ")
        (is (find-card "Ice Wall" (core/all-installed state :corp)))
        (play-from-hand state :corp "Vanilla" "Server 1")
        (is (find-card "Vanilla" (core/all-installed state :corp)))))
    (testing "Upgrades - Root"
      (do-game state
        (play-from-hand state :corp "Research Station" "HQ")
        (is (find-card "Research Station" (core/all-installed state :corp)))))
    (testing "Upgrades - Remotes"
      (do-game state
        (play-from-hand state :corp "Embolus" "Server 1")
        (is (find-card "Embolus" (core/all-installed state :corp)))))
    (testing "Hosted Operations"
      (do-game state
        (play-from-hand state :corp "Ice Wall" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (play-from-hand state :corp "Patch")
        (click-card state :corp "Ice Wall")
        (is (find-card "Patch" (core/all-installed state :corp)))))
    (testing "Currents"
      (do-game state
        (play-from-hand state :corp "Death and Taxes")
        (is (not (find-card "Death and Taxes" (core/all-installed state :corp))))))
    (testing "RFG"
      (do-game state
        (play-from-hand state :corp "Game Changer")
        (is (not (find-card "Game Changer" (core/all-installed state :corp))))))
    (testing "Operation in the play area"
      (do-game state
        (play-from-hand state :corp "Distract the Masses")
        (is (not (find-card "Distract the Masses" (core/all-installed state :corp))))
        (click-prompt state :corp "Done")
        (click-prompt state :corp "Done")))
    (testing "Hand"
      (do-game state
        (is (not (find-card "Enigma" (core/all-installed state :corp))))))
    (testing "Deck"
      (do-game state
        (is (not (find-card "Hedge Fund" (core/all-installed state :corp))))))
    (testing "Discard"
      (do-game state
        (is (not (find-card "IPO" (core/all-installed state :corp))))
        (take-credits state :corp)
        (play-from-hand state :runner "Film Critic")
        (take-credits state :runner)
        (play-from-hand state :corp "MCA Informant")
        (click-card state :corp "Film Critic")))
    (testing "Hosted on runner cards"
      (do-game state
        (is (not (find-card "MCA Informant" (core/all-installed state :corp))))))))
  (testing "runner cards"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Parasite"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Parasite")
      (click-card state :runner "Ice Wall")
      (is (find-card "Parasite" (core/all-installed state :runner)))
      )))
