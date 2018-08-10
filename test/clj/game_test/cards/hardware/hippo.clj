(ns game-test.cards.hardware.hippo
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hippo
  ;; Hippo - remove from game to trash outermost piece of ice if all subs broken
  (testing "No ice"
    (do-game
      (new-game {:runner {:deck ["Hippo"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (empty? (:rfg (get-runner))) "Hippo not RFGed")
      (is (not-empty (get-hardware state)) "Hippo still installed")))
  (testing "Single ice"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Hippo"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Ice Wall installed")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (empty? (get-in @state [:corp :servers :hq :ices])) "Ice Wall removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice Wall trashed")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed")))
  (testing "Multiple ice"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Enigma"]}
                 :runner {:deck ["Hippo"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 2 (count (get-in @state [:corp :servers :hq :ices]))) "2 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall outermost")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma innermost")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Ice removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice trashed")
      (is (= "Ice Wall" (:title (first (:discard (get-corp))))) "Ice Wall in trash")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma still innermost")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed"))))
