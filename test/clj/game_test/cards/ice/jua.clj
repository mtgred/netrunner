(ns game-test.cards.ice.jua
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jua
  ;; Jua
  (testing "Encounter effect - Prevent Runner from installing cards for the rest of the turn"
    (do-game
      (new-game {:corp {:deck ["Jua"]}
                 :runner {:deck ["Desperado" "Sure Gamble"]}})
      (play-from-hand state :corp "Jua" "HQ")
      (take-credits state :corp)
      (let [jua (get-ice state :hq 0)]
        (run-on state "HQ")
        (core/rez state :corp jua)
        (card-ability state :corp (refresh jua) 0)
        (run-successful state)
        (is (= 2 (count (:hand (get-runner)))) "Runner starts with 2 cards in hand")
        (play-from-hand state :runner "Desperado")
        (is (= 2 (count (:hand (get-runner)))) "No cards installed")
        (play-from-hand state :runner "Sure Gamble")
        (is (= 1 (count (:hand (get-runner)))) "Can play events")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:hand (get-runner)))) "Runner starts with 1 cards in hand")
        (play-from-hand state :runner "Desperado")
        (is (zero? (count (:hand (get-runner)))) "Card installed"))))
  (testing "Subroutine effect - Select 2 runner cards, runner moves one to the stack"
    (do-game
      (new-game {:corp {:deck ["Jua"]}
                 :runner {:deck ["Desperado" "Gordian Blade"]}})
      (play-from-hand state :corp "Jua" "HQ")
      (take-credits state :corp)
      (let [jua (get-ice state :hq 0)]
        (core/gain state :runner :credit 10)
        (play-from-hand state :runner "Desperado")
        (run-on state "HQ")
        (core/rez state :corp jua)
        (card-subroutine state :corp (refresh jua) 0)
        (is (empty? (:prompt (get-corp))) "Can't fire for 1 installed card")
        (run-successful state)
        (play-from-hand state :runner "Gordian Blade")
        (run-on state "HQ")
        (card-subroutine state :corp (refresh jua) 0)
        (click-card state :corp (get-program state 0))
        (click-card state :corp (get-hardware state 0))
        (click-prompt state :runner (get-program state 0))
        (is (nil? (get-program state 0)) "Card is uninstalled")
        (is (= 1 (count (:deck (get-runner)))) "Runner puts card in deck"))))
  (testing "Should only lock installing for Runner, not for both sides"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Jua" "Kakugo"]}
                 :runner {:deck ["Paperclip"]}})
      (play-from-hand state :corp "Jua" "HQ")
      (let [mti (get-in @state [:corp :identity])
            jua (get-ice state :hq 0)]
        (core/rez state :corp jua)
        (take-credits state :corp)
        (trash-from-hand state :runner "Paperclip")
        (run-on state "HQ")
        (is (= 1 (get-in @state [:run :position])) "Now approaching Jua")
        (card-ability state :corp jua 0)
        (run-continue state)
        (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
        (card-ability state :corp mti 0)
        (click-card state :corp (find-card "Kakugo" (:hand (get-corp))))
        (is (= 1 (get-in @state [:run :position])) "Now approaching Kakugo")
        (is (= "Kakugo" (:title (get-ice state :hq 0))) "Kakugo was installed")
        (is (empty? (:hand (get-corp))) "Kakugo removed from HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (is (empty? (:prompt (get-runner))) "Runner can't install Paperclip because of Jua encounter ability")
        (run-continue state)
        (is (= 1 (-> (get-runner) :discard count)) "Runner should take 1 net damage from Kakugo")))))
