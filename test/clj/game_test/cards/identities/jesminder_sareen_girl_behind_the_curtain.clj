(ns game-test.cards.identities.jesminder-sareen-girl-behind-the-curtain
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jesminder-sareen-girl-behind-the-curtain
  ;; Jesminder Sareen - avoid tags only during a run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["SEA Source" "Data Raven"]}
                 :runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Data Raven" "Archives")
      (take-credits state :corp)
      (let [dr (-> @state :corp :servers :archives :ices first)]
        (core/rez state :corp dr)
        (core/click-run state :runner {:server "Archives"})
        (card-ability state :corp dr 0)
        (is (zero? (:tag (get-runner))) "Jesminder avoided first tag during the run")
        (card-ability state :corp dr 0)
        (is (= 1 (:tag (get-runner))) "Jesminder did not avoid the second tag during the run")
        (core/no-action state :corp nil)
        (core/continue state :runner nil)
        (core/no-action state :corp nil)
        (core/successful-run state :runner nil)
        (run-empty-server state "R&D") ; clear per-run buffer
        (take-credits state :runner)
        (play-from-hand state :corp "SEA Source")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (:tag (get-runner))) "Jesminder did not avoid the tag outside of a run"))))
  (testing "don't avoid John Masanori tag"
    (do-game
      (new-game {:runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :deck ["John Masanori"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "John Masanori")
      (run-on state "HQ")
      (core/jack-out state :runner nil)
      (is (= 1 (:tag (get-runner))) "Jesminder did not avoid John Masanori tag"))))
