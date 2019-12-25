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
  (testing "Install asset"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (= "PAD Campaign" (:title (get-content state :remote1 0))) "PAD Campaign installed")))
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
  )
