(ns game.core.charge-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer [get-counters]]
   [game.core.charge :as c]
   [game.core.eid :as eid]
   [game.test-framework :refer :all]))

(deftest charge-test
  (testing "Charging a card"
    (do-game
      (new-game {:runner {:hand ["Earthrise Hotel" "Daily Casts"] :credits 10}})
      (take-credits state :corp)
      (is (not (c/can-charge state :runner)) "Can't charge, there are no targets")
      (play-from-hand state :runner "Daily Casts")
      (is (not (c/can-charge state :runner (get-resource state 0))) "Can't charge daily casts")
      (is (not (c/can-charge state :runner)) "Can't charge, there are still no targets")
      (play-from-hand state :runner "Earthrise Hotel")
      (let [hotel (get-resource state 1)]
        (is (= 3 (get-counters (refresh hotel) :power)) "Hotel starts with 3 counters")
        (is (c/can-charge state :runner (refresh hotel)) "We can charge hotel")
        (is (c/can-charge state :runner) "We can charge in general (because of hotel)")
        (is (= 3 (get-counters (refresh hotel) :power)) "Hotel has not been changed")
        (c/charge-card state :runner nil (refresh hotel))
        (is (= 4 (get-counters (refresh hotel) :power)) "Default charge adds 1 power counter")
        (c/charge-card state :runner nil (refresh hotel) 6)
        (is (= 10 (get-counters (refresh hotel) :power)) "4 + 6 = 10 power counters"))
      (let [casts (get-resource state 0)]
        (is (= 0 (get-counters (refresh casts) :power)) "Casts has 0 power counters")
        (c/charge-card state :runner nil (refresh casts))
        (is (= 0 (get-counters (refresh casts) :power)) "Casts still has 0 power counters"))))
  (testing "Charge using a prompt"
    (do-game
      (new-game {:runner {:hand ["Earthrise Hotel" "Daily Casts"] :credits 10}})
      (take-credits state :corp)
      (c/charge-ability state :runner nil nil)
      (is (no-prompt? state :runner) "No prompt to charge because there are no targets")
      (play-from-hand state :runner "Daily Casts")
      (c/charge-ability state :runner nil nil)
      (is (no-prompt? state :runner) "No prompt to charge because there are no valid targets")
      (play-from-hand state :runner "Earthrise Hotel")
      (core/resolve-ability state :runner (eid/make-eid state)
                            (c/charge-ability state :runner nil (eid/make-eid state))
                            (:identity (get-runner)) nil)
      (is (prompt-is-type? state :corp :waiting))
      (click-prompt state :runner "Done")
      (is (no-prompt? state :corp))
      (core/resolve-ability state :runner (eid/make-eid state)
                            (c/charge-ability state :runner nil (eid/make-eid state))
                            (:identity (get-runner)) nil)
      (click-card state :runner "Daily Casts")
      (is (not (no-prompt? state :runner)) "Can't charge Daily Casts")
      (click-card state :runner "Earthrise Hotel")
      (is (= 4 (get-counters (get-resource state 1) :power)))
      (is (no-prompt? state :corp)))))
