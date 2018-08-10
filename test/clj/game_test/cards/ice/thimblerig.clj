(ns game-test.cards.ice.thimblerig
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest thimblerig
  (testing "Thimblerig does not flag phase 1.2 if it's the only piece of ice"
    (do-game
      (new-game {:corp {:deck ["Thimblerig" "Guard"]}})
      (play-from-hand state :corp "Thimblerig" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Corp not in phase 1.2 when Thimblerig is the only piece of ice")
      (play-from-hand state :corp "Guard" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (:corp-phase-12 @state) "Corp in phase 1.2 when there are 2 pieces of ice")))
  (testing "Basic of swap ability - usable both during and outside runs"
    (do-game
      (new-game {:corp {:deck ["Vanilla" "Pup" "Thimblerig"]}})
      (play-from-hand state :corp "Thimblerig" "HQ")
      (play-from-hand state :corp "Pup" "HQ")
      (play-from-hand state :corp "Vanilla" "New remote")
      (let [thimble (get-ice state :hq 0)
            pup (get-ice state :hq 1)]
        (core/rez state :corp thimble)
        (core/rez state :corp pup)
        (is (= "Thimblerig" (:title (get-ice state :hq 0))) "Thimblerig innermost ice on HQ")
        (is (= "Pup" (:title (get-ice state :hq 1))) "Pup outermost ice on HQ")
        (card-ability state :corp (refresh thimble) 0)
        (click-card state :corp (refresh pup))
        (is (= "Pup" (:title (get-ice state :hq 0))) "Pup innermost ice on HQ after swap")
        (is (= "Thimblerig" (:title (get-ice state :hq 1))) "Thimblerig outermost ice on HQ after swap"))
      (let [thimble (get-ice state :hq 1)
            vanilla (get-ice state :remote1 0)]
        (run-on state "Server 1")
        (is (= "Thimblerig" (:title (get-ice state :hq 1))) "Thimblerig outermost ice on HQ")
        (is (= "Vanilla" (:title (get-ice state :remote1 0))) "Vanilla ice on remote")
        (card-ability state :corp thimble 0)
        (click-card state :corp vanilla)
        (is (= "Vanilla" (:title (get-ice state :hq 1))) "Vanilla outermost ice on HQ after swap during run")
        (is (= "Thimblerig" (:title (get-ice state :remote1 0))) "Thimblerig ice on remote after swap during run")))))
