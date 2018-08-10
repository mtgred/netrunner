(ns game-test.cards.assets.city-surveillance
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest city-surveillance
  ;; City Surveillance - Runner chooses to pay 1 credit or take 1 tag at start of their turn
  (do-game
    (new-game {:corp {:deck ["City Surveillance"]}})
    (play-from-hand state :corp "City Surveillance" "New remote")
    (let [surv (get-content state :remote1 0)]
      (core/rez state :corp surv)
      (take-credits state :corp)
      (is (some #{"Pay 1 [Credits]" "Take 1 tag"} (-> (get-runner) :prompt first :choices)))
      (click-prompt state :runner "Pay 1 [Credits]")
      (is (= 4 (:credit (get-runner))) "Runner paid 1 credit")
      (is (zero? (:tag (get-runner))) "Runner didn't take a tag")
      (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")
      (take-credits state :runner)
      (core/lose state :runner :credit (:credit (get-runner))) ;; Set Runner's credits to 0 so they can't choose to pay
      (take-credits state :corp)
      (is (some #{"Take 1 tag"} (-> (get-runner) :prompt first :choices)))
      (click-prompt state :runner "Take 1 tag")
      (is (zero? (:credit (get-runner))) "Runner paid no credits")
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag"))
    (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")))
