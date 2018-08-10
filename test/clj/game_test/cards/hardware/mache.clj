(ns game-test.cards.hardware.mache
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mache
  ;; Mâché
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "PAD Campaign"]}
                 :runner {:deck ["Imp" "Mâché" "Cache"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (starting-hand state :runner ["Imp" "Mâché"])
      (play-from-hand state :runner "Imp")
      (play-from-hand state :runner "Mâché")
      (let [imp (get-program state 0)
            mache (get-hardware state 0)
            counters (get-counters (refresh mache) :power)
            hand (-> (get-runner) :hand count)]
        (run-empty-server state :hq)
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= counters (get-counters (refresh mache) :power)) "Mache should gain no counters from trashing a card with no trash cost")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (is (= (+ counters 4) (get-counters (refresh mache) :power)) "Mache should gain 4 counters for trashing a card with a trash cost of 4")
        (card-ability state :runner mache 0)
        (is (= (inc hand) (-> (get-runner) :hand count)) "Runner should draw one card for using Mache's ability")
        (is (= 1 (get-counters (refresh mache) :power)) "Mache ability should cost 3 counters"))))
  (testing "with Political Operative"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "PAD Campaign"]}
                 :runner {:deck ["Mâché" "Political Operative" "Cache"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (starting-hand state :runner ["Mâché" "Political Operative"])
      (play-from-hand state :runner "Mâché")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Political Operative")
      (take-credits state :runner)
      (let [pad (get-content state :remote1 0)
            mache (get-hardware state 0)
            polop (get-resource state 0)]
        (card-ability state :runner polop 0)
        (click-card state :runner (refresh pad))
        (is (zero? (get-counters (refresh mache) :power)) "Mache should gain no counters from a trash outside of an access")))))
