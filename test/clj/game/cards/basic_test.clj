(ns game.cards.basic-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [card-defs]]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest ^:test-refresh/focus corp-basic-actions
  (testing "Gain 1 credit"
    (do-game
      (new-game)
      (changes-val-macro 1 (:credit (get-corp))
                         "Gain 1 credit"
                         (core/click-credit state :corp nil))))
  (testing "Draw card"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}})
      (changes-val-macro 1 (count (:hand (get-corp)))
                         "Drew 1 card"
                         (core/click-draw state :corp nil))))
  (testing "Install agenda"
    (do-game
      (new-game {:corp {:deck ["Project Beale"]}})
      (play-from-hand state :corp "Project Beale" "New remote")
      (is (= "Project Beale" (:title (get-content state :remote1 0))) "Project Beale installed")))
  (testing "Install asset"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (= "PAD Campaign" (:title (get-content state :remote1 0))) "PAD Campaign installed")))
  (testing "Install upgrade"
    (do-game
      (new-game {:corp {:deck ["Breaker Bay Grid"]}})
      (play-from-hand state :corp "Breaker Bay Grid" "New remote")
      (is (= "Breaker Bay Grid" (:title (get-content state :remote1 0))) "Breaker Bay Grid installed")))
  (testing "Install ice"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (is (= "Ice Wall" (:title (get-ice state :remote1 0))) "Ice Wall installed")))
  (testing "Play operation"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}})
      (changes-val-macro 4 (:credit (get-corp))
                         "Gained 4c from Hedge Fund"
                         (play-from-hand state :corp "Hedge Fund"))))
  )

(deftest ^:test-refresh/focus runner-basic-actions
  (testing "Gain 1 credit"
    (do-game
      (new-game {:options {:start-as :runner}})
      (changes-val-macro 1 (:credit (get-runner))
                         "Gain 1 credit"
                         (core/click-credit state :runner nil))))
  (testing "Draw card"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck [(qty "Sure Gamble" 10)]}})
      (changes-val-macro 1 (count (:hand (get-runner)))
                         "Drew 1 card"
                         (core/click-draw state :runner nil))))
  (testing "Install program"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Misdirection"]}})
      (play-from-hand state :runner "Misdirection")
      (is (= "Misdirection" (:title (get-program state 0))) "Misdirection installed")))
  (testing "Install resource"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Fan Site"]}})
      (play-from-hand state :runner "Fan Site")
      (is (= "Fan Site" (:title (get-resource state 0))) "Fan Site installed")))
  (testing "Install hardware"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Bookmark"]}})
      (play-from-hand state :runner "Bookmark")
      (is (= "Bookmark" (:title (get-hardware state 0))) "Bookmark installed")))
  (testing "Play operation"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck ["Sure Gamble"]}})
      (changes-val-macro 4 (:credit (get-runner))
                         "Gained 4c from Sure Gamble"
                         (play-from-hand state :runner "Sure Gamble"))))
  )
