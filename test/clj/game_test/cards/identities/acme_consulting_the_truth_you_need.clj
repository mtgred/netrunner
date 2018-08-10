(ns game-test.cards.identities.acme-consulting-the-truth-you-need
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest acme-consulting-the-truth-you-need
  (testing "Tag gain when rezzing outermost ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (core/is-tagged? @state)) "Runner does not encounter an unrezzed ice")
      (core/rez state :corp (get-ice state :archives 0))
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (core/is-tagged? @state)) "Runner no longer encountering outermost ice")))
  (testing "Interaction with Data Ward"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Data Ward" (qty "Hedge Fund" 5)]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Data Ward" "Archives")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (core/is-tagged? @state)) "Runner does not encounter an unrezzed ice")
      (core/rez state :corp (get-ice state :archives 0))
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (card-subroutine state :corp (get-ice state :archives 0) 0)
      (is (not (:run @state)) "Run ended by Data Ward")))
  (testing "Tag gain when starting run"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (core/is-tagged? @state)) "Runner no longer encountering outermost ice")))
  (testing "Tag loss when derezzing ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (core/derez state :corp (get-ice state :archives 0))
      (is (not (core/is-tagged? @state)) "Runner no longer encountering the derezzed ice")))
  (testing "No tag on empty server"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (core/is-tagged? @state)) "No ice to encounter")))
  (testing "No tag when encountering second ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Vanilla" 2) (qty "Hedge Fund" 4)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (core/rez state :corp (get-ice state :archives 1))
      (take-credits state :corp)
      (run-on state :archives)
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (core/is-tagged? @state)) "Runner is not tagged when encountering second ice"))))
