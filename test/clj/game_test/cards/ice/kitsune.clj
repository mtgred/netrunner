(ns game-test.cards.ice.kitsune
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kitsune
  (testing "Kitsune - Corp choices card for Runner to access"
    (do-game
      (new-game {:corp {:deck ["Kitsune" "Snare!"]}})
      (play-from-hand state :corp "Kitsune" "R&D")
      (take-credits state :corp)
      (run-on state "R&D")
      (let [kitsune (get-ice state :rd 0)]
        (core/rez state :corp kitsune)
        (card-subroutine state :corp kitsune 0)
        (click-card state :corp (find-card "Snare!" (:hand (get-corp))))
        ;; Runner access Snare! corp has prompt
        (is (= :waiting (-> @state :runner :prompt first :prompt-type))
            "Runner has prompt to wait for Corp to use Snare!")
        (click-prompt state :corp "Yes")
        (is (= "Kitsune" (-> (get-corp) :discard first :title)) "Kitsune was trashed after use")))))
