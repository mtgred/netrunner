(ns game-test.cards.events.tinkering
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tinkering
  ;; Tinkering - Add subtypes to ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Tinkering"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Tinkering")
    (let [iwall (get-ice state :hq 0)]
      (click-card state :runner iwall)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (core/rez state :corp (refresh iwall))
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (take-credits state :runner)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (not (core/has-subtype? (refresh iwall) "Code Gate")) "Ice Wall does not have Code Gate")
      (is (not (core/has-subtype? (refresh iwall) "Sentry")) "Ice Wall does not have Sentry"))))
