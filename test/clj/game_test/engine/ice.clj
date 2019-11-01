(ns game-test.engine.ice
  (:require [game.core :as core]
            [game.utils :as utils]
            [jinteki.utils :as jutils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest auto-pump-and-break
  (testing "update after ice updates subs"
    (do-game
      (new-game {:corp {:hand ["Tour Guide" (qty "PAD Campaign" 2)]
                        :credits 10}
                 :runner {:hand ["Bukhgalter"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Tour Guide" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Bukhgalter")
      (let [p1 (get-content state :remote1 0)
            p2 (get-content state :remote2 0)
            tg (get-ice state :hq 0)
            buk (get-program state 0)]
        (core/rez state :corp p1)
        (core/rez state :corp tg)
        (is (= 1 (count (:subroutines (refresh tg)))))
        (run-on state :hq)
        (is (= "1 [Credits]: Fully break Tour Guide" (-> (refresh buk) :abilities first :label)))
        (core/rez state :corp p2)
        (is (= "2 [Credits]: Fully break Tour Guide" (-> (refresh buk) :abilities first :label)))
        (is (= 2 (count (:subroutines (refresh tg)))))))))

(deftest bioroid-break-abilities
  ;; The click-to-break ablities on bioroids shouldn't create an undo-click
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (run-on state :hq)
    (let [undo-click (:click-state @state)
          clicks (:click (get-runner))]
      (card-side-ability state :runner (get-ice state :hq 0) 0)
      (click-prompt state :runner "End the run")
      (is (= (dec clicks) (:click (get-runner))) "Runner has spent 1 click on the bioroid-break ability")
      (core/command-undo-click state :runner)
      (is (= (inc clicks) (:click (get-runner))) "Runner regains clicks spent on break ability and run")
      (is (not (:run @state)) "Undoing a click resets to before the run began"))))
