(ns game-test.cards.assets.siu
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest siu
  ;; SIU
  (testing "Flags 1.2 and trace for tag with base 3"
    (do-game
      (new-game {:corp {:deck [(qty "SIU" 10)]}})
      (play-from-hand state :corp "SIU" "New remote")
      (let [siu (get-content state :remote1 0)]
        (core/rez state :corp siu)
        (card-ability state :corp (refresh siu) 0) ; try to trigger SIU outside phase 1.2
        (is (= 0 (-> (get-corp) :discard count)) "SIU should not trigger because it's not 1.2")
        (take-credits state :corp)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Corp is in Step 1.2 because SIU is on the table")
        (card-ability state :corp (refresh siu) 0)
        (is (= 1 (-> (get-corp) :discard count)) "SIU should discard to fire trace")
        (is (= 3 (-> (get-corp) :prompt first :base)) "Base Trace should be 3")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (:tag (get-runner))) "Runner has 1 tag")))))
