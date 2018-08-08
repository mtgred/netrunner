(ns game-test.cards.assets.net-police
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest net-police
  ;; Net Police - Recurring credits equal to Runner's link
  (do-game
    (new-game {:corp {:deck ["Net Police"]}
               :runner {:id "Sunny Lebeau: Security Specialist"
                        :deck ["Dyson Mem Chip"
                               "Access to Globalsec"]}})
    (play-from-hand state :corp "Net Police" "New remote")
    (is (= 2 (:link (get-runner))))
    (let [netpol (get-content state :remote1 0)]
      (core/rez state :corp netpol)
      (is (= 2 (get-counters (refresh netpol) :recurring)) "2 recurring for Runner's 2 link")
      (take-credits state :corp)
      (play-from-hand state :runner "Dyson Mem Chip")
      (take-credits state :runner)
      (is (= 3 (get-counters (refresh netpol) :recurring)) "3 recurring for Runner's 3 link")
      (take-credits state :corp)
      (play-from-hand state :runner "Access to Globalsec")
      (take-credits state :runner)
      (is (= 4 (get-counters (refresh netpol) :recurring)) "4 recurring for Runner's 4 link"))))
