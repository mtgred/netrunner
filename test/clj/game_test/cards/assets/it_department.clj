(ns game-test.cards.assets.it-department
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest it-department
  ;; IT Department - Add strength to rezzed ICE until end of turn
  (do-game
    (new-game {:corp {:deck ["IT Department" "Wall of Static"]}})
    (play-from-hand state :corp "IT Department" "New remote")
    (play-from-hand state :corp "Wall of Static" "Server 1")
    (let [itd (get-content state :remote1 0)
          wos (get-ice state :remote1 0)]
      (core/rez state :corp itd)
      (core/rez state :corp wos)
      (card-ability state :corp itd 1)
      (is (zero? (:click (get-corp))) "Spent 1 click")
      (is (= 1 (get-counters (refresh itd) :power)) "IT Dept has 1 counter")
      (core/add-counter state :corp (refresh itd) :power 4)
      (is (= 5 (get-counters (refresh itd) :power)) "IT Dept has 5 counters")
      (card-ability state :corp itd 0)
      (click-card state :corp wos)
      ;; refer to online guides for summary of how this ludicrous formula is calculated
      (is (= 8 (:current-strength (refresh wos))) "Gained 5 strength")
      (is (= 4 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 0)
      (click-card state :corp wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 3 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 0)
      (click-card state :corp wos)
      (is (= 12 (:current-strength (refresh wos))) "Gained total of 9 strength")
      (is (= 2 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :corp itd 0)
      (click-card state :corp wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 1 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (take-credits state :corp)
      (is (= 3 (:current-strength (refresh wos))) "Back to default strength"))))
