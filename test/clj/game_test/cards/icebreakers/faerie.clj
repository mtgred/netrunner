(ns game-test.cards.icebreakers.faerie
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest faerie
  ;; Faerie - trash after encounter is over, not before.
  (do-game
    (new-game {:corp {:deck ["Caduceus"]}
               :runner {:deck ["Faerie"]}})
    (play-from-hand state :corp "Caduceus" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Faerie")
    (let [fae (get-program state 0)]
      (run-on state :archives)
      (core/rez state :corp (get-ice state :archives 0))
      (card-ability state :runner fae 0)
      (is (refresh fae) "Faerie not trashed until encounter over")
      (run-continue state)
      (is (find-card "Faerie" (:discard (get-runner))) "Faerie trashed"))))
