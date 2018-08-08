(ns game-test.cards.identities.ken-express-tenma-disappeared-clone
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ken-express-tenma-disappeared-clone
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game {:runner {:id "Ken \"Express\" Tenma: Disappeared Clone"
                        :deck [(qty "Account Siphon" 2)]}})
    (take-credits state :corp)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit for first Run event")
    (click-prompt state :runner "Replacement effect")
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 16 (:credit (get-runner))) "No credit gained for second Run event")))
