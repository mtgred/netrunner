(ns game-test.engine.ice
  (:require [game.core :as core]
            [game.utils :as utils]
            [jinteki.utils :as jutils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
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
        (is (= "1 [Credits]: Fully break Tour Guide" (-> (refresh buk) :abilities first :label)))
        (core/rez state :corp p2)
        (is (= "2 [Credits]: Fully break Tour Guide" (-> (refresh buk) :abilities first :label)))
        (is (= 2 (count (:subroutines (refresh tg)))))))))
