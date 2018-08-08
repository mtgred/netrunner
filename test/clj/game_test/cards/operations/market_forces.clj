(ns game-test.cards.operations.market-forces
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest market-forces
  (testing "Full test"
    (letfn [(market-forces-credit-test
              [{:keys [tag-count runner-creds expected-credit-diff]}]
              (testing (str "when the runner has " tag-count " tags and " runner-creds " credits")
                (do-game
                  (new-game {:corp {:deck [(qty "Market Forces" 6)]}})
                  (swap! state assoc-in [:corp :credit] 0)
                  (swap! state assoc-in [:runner :credit] runner-creds)
                  (core/gain state :runner :tag tag-count)
                  (play-from-hand state :corp "Market Forces")
                  (is (= expected-credit-diff (:credit (get-corp)))
                      (str "the corp gains " expected-credit-diff " credits"))
                  (is (= expected-credit-diff (- runner-creds (:credit (get-runner))))
                      (str "the runner loses " expected-credit-diff " credits")))))]
      (doall (map market-forces-credit-test
                  [{:tag-count            1
                    :runner-creds         10
                    :expected-credit-diff 3}
                   {:tag-count            2
                    :runner-creds         10
                    :expected-credit-diff 6}
                   {:tag-count            3
                    :runner-creds         10
                    :expected-credit-diff 9}
                   {:tag-count            3
                    :runner-creds         0
                    :expected-credit-diff 0}
                   {:tag-count            3
                    :runner-creds         5
                    :expected-credit-diff 5}]))))
  (testing "when the runner is not tagged"
    (do-game
      (new-game {:corp {:deck [(qty "Market Forces" 6)]}})
      (play-from-hand state :corp "Market Forces")
      (is (= 6 (count (:hand (get-corp))))
          "Market Forces is not played")
      (is (= 3 (:click (get-corp)))
          "the corp does not spend a click")
      (is (= 5 (:credit (get-corp)) (:credit (get-runner)))
          "credits are unaffected"))))
