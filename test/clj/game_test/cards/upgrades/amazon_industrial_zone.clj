(ns game-test.cards.upgrades.amazon-industrial-zone
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest amazon-industrial-zone
  ;; Amazon Industrial Zone - Immediately rez ICE installed over its server at 3 credit discount
  (do-game
    (new-game {:corp {:deck ["Spiderweb" "Amazon Industrial Zone"]}})
    (take-credits state :corp 1)
    (play-from-hand state :corp "Amazon Industrial Zone" "New remote")
    (let [aiz (get-content state :remote1 0)]
      (core/rez state :corp aiz)
      (is (= 2 (:credit (get-corp))))
      (play-from-hand state :corp "Spiderweb" "Server 1")
      (click-prompt state :corp "Yes") ; optional ability
      (let [spid (get-ice state :remote1 0)]
        (is (:rezzed (refresh spid)) "Spiderweb rezzed")
        (is (= 1 (:credit (get-corp))) "Paid only 1 credit to rez")))))
