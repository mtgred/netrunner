(ns game-test.cards.identities.titan-transnational-investing-in-your-future
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest titan-transnational-investing-in-your-future
  ;; Titan Transnational
  (testing "Add a counter to a scored agenda"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atl (get-content state :remote1 0)]
        (core/gain state :corp :click 1)
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/score state :corp {:card (refresh atl)})
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))
  (testing "only use one counter of Corporate Sales Team"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Corporate Sales Team" "Mark Yale"]}})
      (play-from-hand state :corp "Corporate Sales Team" "New remote")
      (play-from-hand state :corp "Mark Yale" "New remote")
      (let [cst (get-content state :remote1 0)
            my (get-content state :remote2 0)]
        (core/gain state :corp :click 3)
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/score state :corp {:card (refresh cst)})
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
          (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
          (core/rez state :corp my)
          (card-ability state :corp my 1)
          (click-card state :corp (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
          (card-ability state :corp my 1)
          (click-card state :corp (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale"))))))
