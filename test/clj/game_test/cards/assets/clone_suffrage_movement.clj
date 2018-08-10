(ns game-test.cards.assets.clone-suffrage-movement
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest clone-suffrage-movement
  ;; Clone Suffrage Movement
  (do-game
    (new-game {:corp {:deck ["Clone Suffrage Movement" (qty "Hedge Fund" 2) "Ice Wall"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Clone Suffrage Movement" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (let [csm (get-content state :remote1 0)]
      (core/rez state :corp (refresh csm))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 2 (-> (get-corp) :discard count)) "Clone Suffrage Movement should activate")
      (is (:corp-phase-12 @state) "Corp should get option to fire Clone Suffrage Movement")
      ;; Runner has 1+ credit and chooses to pay 1 credit
      (card-ability state :corp csm 0)
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp))))
      (core/end-phase-12 state :corp nil)
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Clone Suffrage Movement didn't activate cuz of the ice"))))
