(ns game-test.cards.assets.synth-dna-modification
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest synth-dna-modification
  ;; Synth DNA Modification
  (do-game
    (new-game {:corp {:deck ["Synth DNA Modification" "Data Mine"]}})
    (play-from-hand state :corp "Synth DNA Modification" "New remote")
    (play-from-hand state :corp "Data Mine" "HQ")
    (let [dna (get-content state :remote1 0)
          data (get-ice state :hq 0)]
      (core/rez state :corp dna)
      (core/rez state :corp data)
      (take-credits state :corp)
      (run-on state "HQ")
      (card-subroutine state :corp data 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner should take 1 net damage from Data Mine")
      (is (= 1 (count (:discard (get-corp)))) "Data Mine should trash self after subroutine fires")
      (card-ability state :corp dna 0)
      (is (= 2 (count (:discard (get-runner))))
          "Runner should take 1 net damage from Synth DNA Modification after Data Mine subroutine"))))
