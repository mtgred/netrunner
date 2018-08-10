(ns game-test.cards.ice.hydra
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hydra
  ;; Hydra - do an effect Runner is tagged, otherwise give Runner 1 tag
  (do-game
    (new-game {:corp {:deck ["Hydra"]}})
    (play-from-hand state :corp "Hydra" "HQ")
    (take-credits state :corp)
    (core/gain-credits state :corp 10)
    (run-on state :hq)
    (let [hydra (get-ice state :hq 0)
          corp-creds (:credit (get-corp))]
      (core/rez state :corp hydra)
      (is (= (- corp-creds 10) (:credit (get-corp))) "Cost 10 credits to rez Hydra")
      (is (not (core/is-tagged? @state)) "Runner is not tagged approaching Hydra")
      (testing "Hydra subroutines give tags if Runner is not tagged"
        (doseq [n (range 3)]
          (card-subroutine state :corp hydra n)
          (is (= 1 (:tag (get-runner))) (str "Hydra sub " (inc n) " gave Runner 1 tag"))
          (core/lose state :runner :tag 1)))
      (testing "Hydra subroutines do their effect if the Runner is tagged"
        ;; Gain 1 tag to turn on main effect of subroutines
        (core/gain state :runner :tag 1)
        (is (core/is-tagged? @state) "Runner is tagged")
        (is (= 3 (count (:hand (get-runner)))) "3 cards in Runner grip before Hydra damage")
        (card-subroutine state :corp hydra 0)
        (is (= 0 (count (:hand (get-runner)))) "Hydra sub 1 did 3 damage when Runner is tagged")
        (card-subroutine state :corp hydra 1)
        (is (= (- corp-creds 5) (:credit (get-corp))) "Hydra sub 2 gave 5 credits to Corp when Runner is tagged")
        (is (:run @state) "Still a run going on before resolving last subroutine")
        (card-subroutine state :corp hydra 2)
        (is (not (:run @state)) "Hydra sub 3 ended the run when Runner is tagged")))))
