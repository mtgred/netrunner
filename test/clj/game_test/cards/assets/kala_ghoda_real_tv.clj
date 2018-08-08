(ns game-test.cards.assets.kala-ghoda-real-tv
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kala-ghoda-real-tv
  ;; Kala Ghoda Real TV
  (do-game
    (new-game {:corp {:deck ["Kala Ghoda Real TV"]}})
    (starting-hand state :runner ["Sure Gamble"])
    (play-from-hand state :corp "Kala Ghoda Real TV" "New remote")
    (let [tv (get-content state :remote1 0)]
      (core/rez state :corp tv)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :corp tv 0)
      (click-prompt state :corp "OK")
      (card-ability state :corp tv 1)
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 1 (count (:discard (get-runner)))))
      (is (last-log-contains? state "Sure Gamble")
          "Kala Ghoda did log trashed card names"))))
