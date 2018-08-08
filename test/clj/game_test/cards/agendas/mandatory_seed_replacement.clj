(ns game-test.cards.agendas.mandatory-seed-replacement
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mandatory-seed-replacement
  ;; Mandatory Seed Replacement
  (do-game
    (new-game {:corp {:deck ["Mandatory Seed Replacement"
                             "Ice Wall" "Fire Wall"
                             "Kakugo" "Chum"
                             "RSVP" "Sensei"]}})
    (core/click-draw state :corp 2)
    (core/gain state :corp :click 10 :credit 10)
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Fire Wall" "R&D")
    (play-from-hand state :corp "Kakugo" "HQ")
    (play-from-hand state :corp "Chum" "Archives")
    (play-from-hand state :corp "RSVP" "R&D")
    (play-from-hand state :corp "Sensei" "HQ")
    (let [iw (get-ice state :archives 0)
          fw (get-ice state :rd 0)
          kk (get-ice state :hq 0)
          ch (get-ice state :archives 1)
          rs (get-ice state :rd 1)
          sn (get-ice state :hq 1)]
      (core/rez state :corp iw)
      (core/rez state :corp fw)
      (core/rez state :corp kk)
      (core/rez state :corp ch)
      (core/rez state :corp rs)
      (core/rez state :corp sn)
      (play-and-score state "Mandatory Seed Replacement")
      (click-card state :corp (refresh iw))
      (click-card state :corp (refresh fw))
      (click-card state :corp (refresh kk))
      (click-card state :corp (refresh ch))
      (click-card state :corp (refresh rs))
      (click-card state :corp (refresh sn)))))
