(ns game-test.cards.assets.ronin
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ronin
  ;; Ronin - Click-trash to do 3 net damage when it has 4 or more advancements
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ronin" "Mushin No Shin"]}})
      (play-from-hand state :corp "Mushin No Shin")
      (click-card state :corp (find-card "Ronin" (:hand (get-corp))))
      (let [ron (get-content state :remote1 0)]
        (is (= 3 (get-counters (refresh ron) :advancement)))
        (core/rez state :corp (refresh ron))
        (card-ability state :corp ron 0)
        (is (= 3 (count (:hand (get-runner)))) "Ronin ability didn't fire with only 3 advancements")
        (take-credits state :corp)
        (take-credits state :runner)
        (core/advance state :corp {:card (refresh ron)})
        (is (= 4 (get-counters (refresh ron) :advancement)))
        (card-ability state :corp ron 0)
        (is (= 3 (count (:discard (get-runner)))) "Ronin did 3 net damage")
        (is (= 2 (count (:discard (get-corp)))) "Ronin trashed"))))
  (testing "doesn't fire (or crash) if no advance counters"
    (do-game
      (new-game {:corp {:deck ["Ronin"]}})
      (play-from-hand state :corp "Ronin" "New remote")
      (let [ron (get-content state :remote1 0)]
        (is (zero? (get-counters (refresh ron) :advancement)) "Ronin starts with no counters")
        (core/rez state :corp (refresh ron))
        (card-ability state :corp (refresh ron) 0)
        (is (zero? (get-counters (refresh ron) :advancement)) "Ronin didn't gain counters")
        (is (= 3 (count (:hand (get-runner)))) "Ronin ability didn't fire with 0 advancements")))))
