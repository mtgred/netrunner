(ns game-test.cards.identities.kate-mac-mccaffrey-digital-tinker
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kate-mac-mccaffrey-digital-tinker
  ;; Kate 'Mac' McCaffrey
  (testing "Install discount"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))
  (testing "No discount for 0 cost"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"
                                 "Self-modifying Code"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Magnum Opus")
      (is (zero? (:credit (get-runner))) "No Kate discount on second program install")))
  (testing "Can afford only with the discount"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (core/lose state :runner :credit 1)
      (is (= 4 (:credit (get-runner))))
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (count (get-program state))) "Magnum Opus installed")
      (is (zero? (:credit (get-runner))) "Installed Magnum Opus for 4 credits"))))
