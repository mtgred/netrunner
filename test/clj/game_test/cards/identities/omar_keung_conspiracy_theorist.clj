(ns game-test.cards.identities.omar-keung-conspiracy-theorist
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest omar-keung-conspiracy-theorist
  ;; Omar Keung
  (testing "Make a successful run on the chosen server once per turn"
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])]
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (click-prompt state :runner "No action")
        (is (= 3 (:click (get-runner))))
        (card-ability state :runner omar 0)
        (is (= 3 (:click (get-runner))))
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :rd)
        (is (= [:rd] (get-in @state [:runner :register :successful-run])))
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= [:hq :rd] (get-in @state [:runner :register :successful-run]))))))
  (testing "Ash prevents access, but not successful run"
    (do-game
      (new-game {:corp {:deck ["Ash 2X3ZB9CY"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            ash (get-content state :hq 0)]
        (core/rez state :corp ash)
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)))
        (is (= :hq (-> (get-runner) :register :successful-run first))))))
  (testing "Crisium Grid prevents prompt"
    (do-game
      (new-game {:corp {:deck ["Crisium Grid"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            cr (get-content state :archives 0)]
        (core/rez state :corp cr)
        (card-ability state :runner omar 0)
        (run-successful state)
        (is (= (:cid cr) (-> (get-runner) :prompt first :card :cid)))
        (is (empty? (-> (get-runner) :register :successful-run)))
        (is (= :archives (get-in @state [:run :server 0]))))))
  (testing "When selecting R&D, ability adds counters to Medium"
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck ["Medium"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Medium")
      (let [omar (get-in @state [:runner :identity])
            medium (get-program state 0)]
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "R&D")
        (is (= 1 (get-counters (refresh medium) :virus))))))
  (testing "When selecting HQ, ability adds counters to Nerve Agent"
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck ["Nerve Agent"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Nerve Agent")
      (let [omar (get-in @state [:runner :identity])
            nerve (get-program state 0)]
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= 1 (get-counters (refresh nerve) :virus)))))))
