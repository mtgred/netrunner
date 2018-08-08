(ns game-test.cards.resources.the-black-file
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-black-file
  ;; The Black File - Prevent Corp from winning by agenda points
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Vanity Project" 3) (qty "Sure Gamble" 3)]}
                 :runner {:deck ["The Black File"]}})
      (starting-hand state :corp ["Vanity Project"])
      (core/gain state :corp :agenda-point 3)
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (take-credits state :runner)
      (play-from-hand state :corp "Vanity Project" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 7 (:agenda-point (get-corp))))
      (is (not (:winner @state)) "No registered Corp win")
      (take-credits state :corp)
      (let [bf (get-resource state 0)]
        (is (= 1 (get-counters (refresh bf) :power)) "1 power counter on The Black File")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh bf) :power)) "2 power counters on The Black File")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:rfg (get-runner)))) "The Black File removed from the game")
        (is (= :corp (:winner @state)) "Corp wins")
        (is (= "Agenda" (:reason @state)) "Win condition reports agendas"))))
  (testing "Corp can still win by flatlining Runner"
    (do-game
      (new-game {:corp {:deck [(qty "Vanity Project" 3) (qty "Scorched Earth" 3)]}
                 :runner {:deck ["The Black File"]}})
      (starting-hand state :corp ["Vanity Project" "Scorched Earth"])
      (core/gain state :corp :agenda-point 3)
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (take-credits state :runner)
      (play-from-hand state :corp "Vanity Project" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 7 (:agenda-point (get-corp))))
      (is (not (:winner @state)) "No registered Corp win")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
