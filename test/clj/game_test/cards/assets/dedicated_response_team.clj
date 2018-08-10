(ns game-test.cards.assets.dedicated-response-team
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest dedicated-response-team
  ;; Dedicated Response Team - Do 2 meat damage when successful run ends if Runner is tagged
  (do-game
    (new-game {:corp {:deck ["Dedicated Response Team"]}})
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (let [drt (get-content state :remote1 0)]
      (core/rez state :corp drt)
      (take-credits state :corp)
      (run-empty-server state :rd)
      (is (empty? (:discard (get-runner))) "Not tagged, no damage done")
      (core/gain state :runner :tag 1)
      (run-on state :rd)
      (run-jack-out state)
      (is (empty? (:discard (get-runner))) "Tagged but run unsuccessful, no damage done")
      (run-empty-server state :rd)
      (is (= 2 (count (:discard (get-runner)))) "Suffered 2 damage for successful run w/ tag"))))
