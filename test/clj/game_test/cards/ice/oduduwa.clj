(ns game-test.cards.ice.oduduwa
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest oduduwa
  ;; Oduduwa - Gain 1 advancement token when encountered.
  ;; May placed x advancement tokens on another ice where x is the number of counters on Oduduwa already.
  (do-game
    (new-game {:corp {:deck ["Oduduwa" "Enigma"]}})
    (play-from-hand state :corp "Oduduwa" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (let [odu (get-ice state :hq 0)
          eni (get-ice state :rd 0)]
      (core/rez state :corp odu)
      (core/rez state :corp eni)
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :corp (refresh odu) 0)
      (card-ability state :corp (refresh odu) 1)
      (click-card state :corp (refresh eni))
      (is (= 1 (get-counters (refresh odu) :advancement)))
      (is (= 1 (get-counters (refresh eni) :advancement)))
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :corp (refresh odu) 0)
      (card-ability state :corp (refresh odu) 1)
      (click-card state :corp (refresh eni))
      (is (= 2 (get-counters (refresh odu) :advancement)))
      (is (= 3 (get-counters (refresh eni) :advancement)))
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :corp (refresh odu) 0)
      (card-ability state :corp (refresh odu) 1)
      (click-card state :corp (refresh eni))
      (is (= 3 (get-counters (refresh odu) :advancement)))
      (is (= 6 (get-counters (refresh eni) :advancement))))))
