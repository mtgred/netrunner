(ns game-test.cards.ice.magnet
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest magnet
  ;; Magnet - host program when rezzed
  (testing "Faceup ice"
    (do-game
      (new-game {:corp {:deck ["Magnet" "Enigma"]}
                 :runner {:deck ["Parasite"]}})
      (play-from-hand state :corp "Magnet" "HQ")
      (play-from-hand state :corp "Enigma" "R&D")
      (core/rez state :corp (get-ice state :rd 0))
      (take-credits state :corp)
      (let [m (get-ice state :hq 0)
            e (get-ice state :rd 0)]
        (play-from-hand state :runner "Parasite")
        (click-card state :runner (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
        (run-on state "HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (click-card state :corp (first (:hosted (get-ice state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
        (is (= 1 (count (:hosted (refresh m)))) "Parasite hosted on Magnet")
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (zero? (core/get-virus-counters state :runner (first (:hosted (refresh m)))))
            "Parasite does not gain a virus counter"))))
  (testing "Facedown ice"
    (do-game
      (new-game {:corp {:deck ["Magnet" "Enigma"]}
                 :runner {:deck ["Trypano"]}})
      (play-from-hand state :corp "Magnet" "HQ")
      (play-from-hand state :corp "Enigma" "R&D")
      (take-credits state :corp)
      (let [m (get-ice state :hq 0)
            e (get-ice state :rd 0)]
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Trypano hosted on Enigma")
        (run-on state "HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (click-card state :corp (first (:hosted (get-ice state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Trypano")
        (is (= 1 (count (:hosted (refresh m)))) "Trypano hosted on Magnet")
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (empty? (:prompt (get-runner))) "No Trypano prompt")
        (is (zero? (core/get-virus-counters state :runner (first (:hosted (refresh m)))))
            "Trypano does not gain a virus counter"))))
  (testing "Derezzed ice"
    (do-game
      (new-game {:corp {:deck ["Magnet" "Enigma"]}
                 :runner {:deck [(qty "Parasite" 2)]}})
      (play-from-hand state :corp "Magnet" "HQ")
      (play-from-hand state :corp "Enigma" "R&D")
      (core/rez state :corp (get-ice state :rd 0))
      (take-credits state :corp)
      (let [m (get-ice state :hq 0)
            e (get-ice state :rd 0)]
        (play-from-hand state :runner "Parasite")
        (click-card state :runner (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
        (run-on state "HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (click-card state :corp (first (:hosted (get-ice state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
        (is (= 1 (count (:hosted (refresh m)))) "Parasite hosted on Magnet")
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (zero? (core/get-virus-counters state :runner (first (:hosted (refresh m)))))
            "Parasite does not gain a virus counter")
        (take-credits state :runner)
        (core/derez state :corp (refresh m))
        (take-credits state :corp)
        (is (= 1 (core/get-virus-counters state :runner (first (:hosted (refresh m)))))
            "Parasite gains a virus counter on derezzed Magnet")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
        (run-on state "HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (click-card state :corp (first (:hosted (get-ice state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
        (is (= 2 (count (:hosted (refresh m)))) "Parasites hosted on Magnet")
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (core/get-virus-counters state :runner (first (:hosted (refresh m)))))
            "First parasite stays at 1 virus counter on rezzed Magnet")
        (is (zero? (core/get-virus-counters state :runner (second (:hosted (refresh m)))))
            "Second parasite does not gain a virus counter on derezzed Magnet")
        (take-credits state :runner)
        (core/derez state :corp (refresh m))
        (take-credits state :corp)
        (is (= 2 (core/get-virus-counters state :runner (first (:hosted (refresh m)))))
            "First parasite gains a virus counter on derezzed Magnet")
        (is (= 1 (core/get-virus-counters state :runner (second (:hosted (refresh m)))))
            "Second parasite gains a virus counter on rezzed Magnet")))))
