(ns game-test.cards.resources.citadel-sanctuary
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest citadel-sanctuary
  (testing "Interaction with Corporate Grant and Thunder Art Gallery"
    (do-game
      (new-game {:runner {:deck ["Citadel Sanctuary" "Thunder Art Gallery" "Corroder" "Corporate \"Grant\""]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Citadel Sanctuary")
      (play-from-hand state :runner "Thunder Art Gallery")
      (play-from-hand state :runner "Corporate \"Grant\"")
      (take-credits state :runner)
      (take-credits state :corp)
      (core/gain-tags state :runner 1)
      (core/lose state :runner :click 3)
      (core/end-turn state :runner nil)
      (is (= 11 (:credit (get-corp))) "Corp has 11 credits before Corporate Grant")
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      (is (not (:end-turn @state)) "Runner turn has not yet ended")
      (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
      (is (:end-turn @state) "Runner turn has now ended")
      (is (= 10 (:credit (get-corp))) "Corp lost 1 credit to Corporate Grant"))))
