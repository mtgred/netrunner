(ns game.engine.ice-test
  (:require [game.core :as core]
            [game.utils :as utils]
            [jinteki.utils :as jutils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
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
        (is (= "1 [Credits]: break 1 Sentry subroutine" (-> (refresh buk) :abilities first :label))
            "Not encountered an ice yet")
        (core/rez state :corp p2)
        (run-continue state)
        (is (= "2 [Credits]: Fully break Tour Guide" (-> (refresh buk) :abilities first :label)))
        (is (= 2 (count (:subroutines (refresh tg))))))))
  (testing "Also works on second encounter"
    (do-game
      (new-game {:corp {:hand ["Tour Guide" (qty "PAD Campaign" 2)]
                        :credits 10}
                 :runner {:hand ["Bukhgalter"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Tour Guide" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Bukhgalter")
      (let [p1 (get-content state :remote1 0)
            tg (get-ice state :hq 0)
            buk (get-program state 0)]
        (core/rez state :corp p1)
        (core/rez state :corp tg)
        (run-on state :hq)
        (run-continue state)
        (is (= "1 [Credits]: Fully break Tour Guide" (-> (refresh buk) :abilities first :label)))
        (fire-subs state tg)
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :hq)
        (run-continue state)
        (is (= "1 [Credits]: Fully break Tour Guide" (-> (refresh buk) :abilities first :label))))))
  (testing "Breaking restrictions on auto-pump-and-break - No auto pumping if (:breakable sub) does not return :unrestricted"
    (do-game
      (new-game {:corp {:hand ["Afshar"]}
                 :runner {:hand ["Gordian Blade"]
                          :credits 10}})
      (play-from-hand state :corp "Afshar" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :hq)
      (let [afshar (get-ice state :hq 0)
            gord (get-program state 0)]
        (core/rez state :corp afshar)
        (run-continue state)
        (is (empty? (filter #(= :auto-pump-and-break (:dynamic %)) (:abilities (refresh gord)))) "No auto break dynamic ability"))))
  (testing "Breaking restrictions on auto-pump-and-break - Auto pumping if (:breakable sub) returns :unrestricted"
    (do-game
      (new-game {:corp {:hand ["Afshar"]}
                 :runner {:hand ["Gordian Blade"]
                          :credits 10}})
      (play-from-hand state :corp "Afshar" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Gordian Blade")
      (run-on state :rd)
      (let [afshar (get-ice state :rd 0)
            gord (get-program state 0)]
        (core/rez state :corp afshar)
        (run-continue state)
        (is (not-empty (filter #(= :auto-pump-and-break (:dynamic %)) (:abilities (refresh gord)))) "Autobreak is active")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh gord)})
        (is (empty? (remove :broken (:subroutines (refresh afshar)))) "All subroutines broken")))))

(deftest bioroid-break-abilities
  ;; The click-to-break ablities on bioroids shouldn't create an undo-click
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (core/rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (let [undo-click (:click-state @state)
          clicks (:click (get-runner))]
      (card-side-ability state :runner (get-ice state :hq 0) 0)
      (click-prompt state :runner "End the run")
      (is (= (dec clicks) (:click (get-runner))) "Runner has spent 1 click on the bioroid-break ability")
      (core/command-undo-click state :runner)
      (is (= (inc clicks) (:click (get-runner))) "Runner regains clicks spent on break ability and run")
      (is (not (:run @state)) "Undoing a click resets to before the run began"))))
