(ns game-test.cards.ice.mausolus
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mausolus
  ;; Mausolus - 3 adv tokens change the subroutines
  (do-game
    (new-game {:corp {:deck ["Mausolus"]}
               :runner {:deck [(qty "NetChip" 5)]}})
    (play-from-hand state :corp "Mausolus" "HQ")
    (let [mau (get-ice state :hq 0)]
      (core/rez state :corp mau)
      (take-credits state :corp)
      (run-on state :hq)
      (is (= 3 (:credit (get-corp))) "corp starts encounter with 3 crs")
      (is (zero? (count (:discard (get-runner)))) "runner starts encounter with no cards in heap")
      (is (zero? (:tag (get-runner))) "runner starts encounter with 0 tags")
      (card-subroutine state :corp mau 0)
      (card-subroutine state :corp mau 1)
      (card-subroutine state :corp mau 2)
      (is (= 4 (:credit (get-corp))) "corp gains 1 cr from mausolus")
      (is (= 1 (count (:discard (get-runner)))) "corp does 1 net damage")
      (is (= 1 (:tag (get-runner))) "corp gives 1 tag")
      (run-jack-out state)
      (take-credits state :runner)
      (core/advance state :corp {:card (refresh mau)})
      (core/advance state :corp {:card (refresh mau)})
      (core/advance state :corp {:card (refresh mau)})
      (run-on state :hq)
      (is (= 1 (:credit (get-corp))) "corp starts encounter with 1 crs")
      (is (= 1 (count (:discard (get-runner)))) "runner starts encounter with 1 card in heap")
      (is (= 1 (:tag (get-runner))) "runner starts encounter with 1 tags")
      (card-subroutine state :corp mau 0)
      (card-subroutine state :corp mau 1)
      (card-subroutine state :corp mau 2)
      (is (= 4 (:credit (get-corp))) "corp gains 3 cr")
      (is (= 4 (count (:discard (get-runner)))) "corp does 3 net damage")
      (is (= 2 (:tag (get-runner))) "corp gives 1 tag")
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:runner :register :unsuccessful-run]) "Run was unsuccessful"))))
