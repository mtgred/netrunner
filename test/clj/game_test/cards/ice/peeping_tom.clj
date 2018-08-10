(ns game-test.cards.ice.peeping-tom
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest peeping-tom
  ;;Peeping Tom - Counts # of chosen card type in Runner grip
  (do-game
    (new-game {:corp {:deck ["Peeping Tom"]}
               :runner {:deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Peeping Tom" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [tom (get-ice state :hq 0)]
      (core/rez state :corp (refresh tom))
      (card-ability state :corp tom 0)
      (click-prompt state :corp "Hardware")
      (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble, Sure Gamble, Sure Gamble")
          "Revealed Runner grip")
      (is (last-log-contains? state "0") "Correctly counted Hardware in Runner grip")
      (card-ability state :corp tom 0)
      (click-prompt state :corp "Event")
      (is (last-log-contains? state "5") "Correctly counted Events in Runner grip")
      (card-side-ability state :runner tom 1)
      (card-side-ability state :runner tom 1)
      (card-side-ability state :runner tom 1)
      (card-side-ability state :runner tom 1)
      (is (= 4 (:tag (get-runner))) "Tag ability sucessful")
      (card-side-ability state :runner tom 0)
      (is (not (:run @state)) "Run ended"))))
