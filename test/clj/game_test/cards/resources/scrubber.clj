(ns game-test.cards.resources.scrubber
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest scrubber
  ;; Scrubber
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["The Board"]}
                 :runner {:deck ["Scrubber"]}})
      (play-from-hand state :corp "The Board" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner doesn't have enough credits to trash")
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Scrubber")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))) "Runner should only have 5 credits in pool")
      (run-empty-server state "Server 1")
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Runner can use Scrubber credits to trash")
      (let [scrubber (get-resource state 0)]
        (card-ability state :runner scrubber 0)
        (card-ability state :runner scrubber 0))
      (click-prompt state :runner "Pay 7 [Credits] to trash")
      (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board and gain 2 agenda points")))
  (testing "when under trash cost but can up with recurring credits"
    (do-game
      (new-game {:corp {:deck ["The Board"]}
                 :runner {:deck ["Scrubber" "Skulljack" "Sure Gamble"]}})
      (play-from-hand state :corp "The Board" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner doesn't have enough credits to trash")
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Scrubber")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Skulljack")
      (core/gain state :runner :credit 1)
      (is (= 4 (:credit (get-runner))) "Runner should only have 4 credits in pool")
      (run-empty-server state "Server 1")
      (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "The Board should cost 6 to trash")
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Runner can use Scrubber credits to trash")
      (click-prompt state :runner "Pay 6 [Credits] to trash") ;; Whoops, runner forgot to actually get the credits from Scrubber
      (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "Skulljack shouldn't trigger a second time")
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Runner can still use Scrubber credits the second time around")
      (let [scrubber (get-resource state 0)]
        (card-ability state :runner scrubber 0)
        (card-ability state :runner scrubber 0))
      (click-prompt state :runner "Pay 6 [Credits] to trash") ;; Now the runner has actually gained the Scrubber credits
      (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board and gain 2 agenda points"))))
