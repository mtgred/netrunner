(ns game-test.cards.assets.psychic-field
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest psychic-field
  (testing "Basic test"
    ;; Psychic Field - Do 1 net damage for every card in Runner's hand when accessed/exposed
    (do-game
      (new-game {:corp {:deck [(qty "Psychic Field" 2)]}
                 :runner {:deck [(qty "Infiltration" 3) (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (play-from-hand state :corp "Psychic Field" "New remote")
      (let [psyf1 (get-content state :remote1 0)
            psyf2 (get-content state :remote2 0)]
        (take-credits state :corp)
        (starting-hand state :runner ["Infiltration" "Sure Gamble" "Sure Gamble"])
        (play-from-hand state :runner "Infiltration")
        (click-prompt state :runner "Expose a card")
        (click-card state :runner psyf1)
        (is (= 2 (count (:hand (get-runner)))))
        (click-prompt state :corp "2 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 3 (count (:discard (get-runner)))) "Suffered 2 net damage on expose and psi loss")
        (core/gain state :runner :click 3)
        (core/draw state :runner 3)
        (is (= 3 (count (:hand (get-runner)))))
        (run-empty-server state :remote2)
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 6 (count (:discard (get-runner)))) "Suffered 3 net damage on access and psi loss"))))
  (testing "when in Archives. #1965"
    (do-game
      (new-game {:corp {:deck [(qty "Psychic Field" 2) (qty "Shock!" 2) (qty "Clone Retirement" 2)]}})
      (trash-from-hand state :corp "Psychic Field")
      (trash-from-hand state :corp "Shock!")
      (trash-from-hand state :corp "Clone Retirement")
      (take-credits state :corp)
      ;; Runner run on archives to trigger access choice
      (run-empty-server state :archives)
      (is (not-any? #{"Psychic Field"} (-> @state :runner :prompt first :choices))
          "Psychic Field is not a choice to access in Archives")))
  (testing "Interaction with Neutralize All Threats and Hostile Infrastructure, #1208"
    (do-game
      (new-game {:corp {:deck [(qty "Psychic Field" 3) (qty "Hostile Infrastructure" 3)]}
                 :runner {:deck ["Neutralize All Threats" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Neutralize All Threats")
      (run-empty-server state :remote1)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (not (get-content state :remote1)) "Psychic Field trashed by Neutralize All Threats")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
