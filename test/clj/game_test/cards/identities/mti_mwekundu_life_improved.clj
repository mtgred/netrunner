(ns game-test.cards.identities.mti-mwekundu-life-improved
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mti-mwekundu-life-improved
  ;; Mti Mwekundu: Life Improved - when server is approached, install ice from HQ at the innermost position
  (testing "No ice"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))
  (testing "Multiple ice"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma" "Ice Wall" "Bloom"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Bloom" "R&D")
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))
  (testing "with Kakugo, passing shouldn't fire net damage twice. #3588"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Kakugo"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "Kakugo" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Kakugo" (:title (get-ice state :hq 0))) "Kakugo was installed")
      (is (empty? (:hand (get-corp))) "Kakugo removed from HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= 1 (-> (get-runner) :discard count)) "Runner should take 1 net damage from Kakugo"))))
