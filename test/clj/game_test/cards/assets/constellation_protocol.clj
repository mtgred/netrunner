(ns game-test.cards.assets.constellation-protocol
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest constellation-protocol
  ;; Constellation Protocol
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Constellation Protocol" "Ice Wall" "Fire Wall"]}})
      (core/gain state :corp :credit 100 :click 10)
      (play-from-hand state :corp "Constellation Protocol" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Fire Wall" "New remote")
      (let [cp (get-content state :remote1 0)
            iw (get-ice state :remote2 0)
            fw (get-ice state :remote3 0)]
        (core/rez state :corp cp)
        (core/rez state :corp iw)
        (core/rez state :corp fw)
        (advance state iw 1)
        (advance state fw 1)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (:corp-phase-12 @state) "Should be waiting for Constellation Protocol to be fired")
        (card-ability state :corp cp 0)
        (is (= 1 (get-counters (refresh iw) :advancement)))
        (is (= 1 (get-counters (refresh fw) :advancement)))
        (click-card state :corp (refresh iw))
        (click-card state :corp (refresh fw))
        (is (zero? (get-counters (refresh iw) :advancement)))
        (is (= 2 (get-counters (refresh fw) :advancement)))
        (core/end-phase-12 state :corp nil))))
  (testing "Variable number of advanceable cards"
    (do-game
      (new-game {:corp {:deck ["Constellation Protocol" "Ice Wall" "Hive"]}})
      (core/gain state :corp :credit 100 :click 10)
      (play-from-hand state :corp "Constellation Protocol" "New remote")
      (let [cp (get-content state :remote1 0)]
        (core/rez state :corp cp))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire with no advanceable ice")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote2 0)]
        (core/rez state :corp iw)
        (advance state iw 1)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire with only a single ice"))
      (play-from-hand state :corp "Hive" "New remote")
      (let [hive (get-ice state :remote3 0)]
        (core/rez state :corp hive)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire when the target ice can't be advanced"))))
  (testing "Can't advance assets"
    (do-game
      (new-game {:corp {:deck ["Constellation Protocol" "Ice Wall" "Contract Killer"]}})
      (core/gain state :corp :credit 100 :click 10)
      (play-from-hand state :corp "Constellation Protocol" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Contract Killer" "New remote")
      (let [cp (get-content state :remote1 0)
            iw (get-ice state :remote2 0)
            ck (get-content state :remote3 0)]
        (core/rez state :corp cp)
        (core/rez state :corp iw)
        (core/rez state :corp ck)
        (advance state iw 1))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Constellation Protocol shouldn't fire when only target is asset"))))
