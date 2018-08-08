(ns game-test.cards.identities.akiko-nisei-head-case
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest akiko-nisei-head-case
  ;; Akiko Nisei
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 2 (core/access-count state :runner :rd-access)) "Should access additional card from ability")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 1 (core/access-count state :runner :rd-access)) "Should only access 1 from missed psi game")))
  (testing "Shiro interaction: second sub should give Akiko 2 accesses"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10) "Shiro"]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (starting-hand state :corp ["Shiro"])
      (play-from-hand state :corp "Shiro" "New remote")
      (let [shiro (get-ice state :remote1 0)]
        (core/rez state :corp shiro)
        (take-credits state :corp)
        (run-on state :remote1)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 2 (core/access-count state :runner :rd-access)) "Should access additional card from ability")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :remote1)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 1 (core/access-count state :runner :rd-access)) "Should only access 1 from missed psi game")))))
