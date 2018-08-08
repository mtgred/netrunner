(ns game-test.cards.assets.ibrahim-salem
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ibrahim-salem
  ;; Ibrahim Salem
  (do-game
    (new-game {:corp {:deck ["Hostile Takeover" "Ibrahim Salem"]}
               :runner {:deck ["Sure Gamble" "Astrolabe" "Paperclip" "Daily Casts"]}})
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Ibrahim Salem" "New remote")
    (let [is (get-content state :remote2 0)]
      (core/rez state :corp (refresh is))
      (click-card state :corp (-> (get-corp) :scored first))
      (doseq [[i [card-type card-name]]
              (map-indexed vector ['("Event" "Sure Gamble")
                                   '("Hardware" "Astrolabe")
                                   '("Program" "Paperclip")
                                   '("Resource" "Daily Casts")])]
        (take-credits state :corp)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp is in Step 1.2")
        (card-ability state :corp is 0)
        (click-prompt state :corp card-type)
        (click-prompt state :corp (find-card card-name (:hand (get-runner))))
        (core/end-phase-12 state :corp nil)
        (is (= (inc i) (-> (get-runner) :discard count)))))))
