(ns game-test.cards.ice.formicary
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest formicary
  ;; Formicary - when approaching server, may rez and move to innermost
  (testing "Verifies basic functionality and that First Responders may trigger"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2) (qty "Formicary" 3)]}
                 :runner {:deck [(qty "First Responders" 6)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Formicary" "Archives")
      (play-from-hand state :corp "Formicary" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "First Responders")
      (let [iw (get-ice state :hq 0)
            form1 (get-ice state :rd 0)
            form2 (get-ice state :archives 0)
            responders (get-resource state 0)]
        (run-on state "HQ")
        (run-continue state)             ; pass the first ice
        (is (= 0 (get-in @state [:run :position])) "Now approaching server")
        (core/rez state :corp form1)
        (click-prompt state :corp "Yes")      ; Move Formicary
        (is (= 2 (count (get-in @state [:corp :servers :hq :ices]))) "2 ICE protecting HQ")
        (is (= 1 (get-in @state [:run :position])) "Now approaching Formicary")
        (card-subroutine state :corp (get-ice state :hq 0) 0)
        (click-prompt state :runner "Yes")      ; take 2 net
        (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage")
        (run-jack-out state)
        (let [cards-in-hand (count (:hand (get-runner)))]
          (card-ability state :runner responders 0)
          (is (= (inc cards-in-hand) (count (:hand (get-runner)))) "First Responders was able to trigger"))
        (run-on state "Archives")
        (run-continue state)
        (core/rez state :corp form2)
        (click-prompt state :corp "Yes")      ; Move Formicary
        (is (= 1 (get-in @state [:run :position])) "Now approaching Formicary")
        (card-subroutine state :corp (refresh form2) 0)
        (click-prompt state :runner "No")      ; ETR
        (is (not (get-in @state [:run])) "Formicary ended the run"))))
  (testing "Verifies that Formicary can be moved to the innermost positon of its own server"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Formicary"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Formicary" "HQ")
      (take-credits state :corp)
      (let [form (get-ice state :hq 1)]
        (run-on state "HQ")
        (run-continue state)             ; pass the first ice
        (run-continue state)             ; pass the second ice
        (is (= 0 (get-in @state [:run :position])) "Now approaching server")
        (core/rez state :corp form)
        (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is the innermost piece of ice before swap")
        (is (= "Formicary" (:title (get-ice state :hq 1))) "Formicary is the outermost piece of ice before swap")
        (click-prompt state :corp "Yes")      ; Move Formicary
        (is (= 1 (get-in @state [:run :position])) "Now approaching the innermost piece of ice")
        (is (= "Formicary" (:title (get-ice state :hq 0))) "Formicary is the innermost piece of ice after swap")
        (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall is the outermost piece of ice after swap")))))
