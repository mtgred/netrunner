(ns game-test.cards.events.rebirth
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

;; Rebirth
(let [akiko "Akiko Nisei: Head Case"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"]
  (deftest rebirth
    ;; Rebirth - Kate's discount applies after rebirth
    (testing "Kate"
      (do-game
        (new-game {:runner {:deck ["Magnum Opus" "Rebirth"]}}
                  {:start-as :runner})
        (play-from-hand state :runner "Rebirth")
        (is (= (first (prompt-titles :runner)) akiko) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [kate kit]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie]))
        (click-prompt state :runner kate)
        (is (= kate (-> (get-runner) :identity :title)))
        (is (= 1 (:link (get-runner))) "1 link")
        (is (empty? (:discard (get-runner))))
        (is (= "Rebirth" (-> (get-runner) :rfg first :title)))
        (is (changes-credits (get-runner) -4
                             (play-from-hand state :runner "Magnum Opus")))))
    (testing "Whizzard works after rebirth"
      (do-game
        (new-game {:corp {:deck ["Ice Wall"]}
                   :runner {:id reina
                            :deck ["Rebirth"]}})
        (play-from-hand state :corp "Ice Wall" "R&D")
        (take-credits state :corp)
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner whizzard)
        (card-ability state :runner (:identity (get-runner)) 0)
        (is (= 6 (:credit (get-runner))) "Took a Whizzard credit")
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-ice state :rd 0)))
            "Reina is no longer active")))
    (testing "Lose link from ID"
      (do-game
        (new-game {:runner {:id kate
                            :deck ["Rebirth" "Access to Globalsec"]}}
                  {:start-as :runner})
        (play-from-hand state :runner "Access to Globalsec")
        (is (= 2 (:link (get-runner))) "2 link before rebirth")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner chaos)
        (is (= 1 (:link (get-runner))) "1 link after rebirth")))
    (testing "Gain link from ID"
      (do-game
        (new-game {:runner {:deck ["Rebirth" "Access to Globalsec"]}}
                  {:start-as :runner})
        (play-from-hand state :runner "Access to Globalsec")
        (is (= 1 (:link (get-runner))) "1 link before rebirth")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner kate)
        (is (= 2 (:link (get-runner))) "2 link after rebirth")))
    (testing "Implementation notes are kept, regression test for #3722"
      (do-game
        (new-game {:runner {:deck ["Rebirth"]}}
                  {:start-as :runner})
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner chaos)
        (is (= :full (get-in (get-runner) [:identity :implementation])) "Implementation note kept as `:full`"))))
  (testing "Rebirth into Kate twice"
    ;; Rebirth - Kate does not give discount after rebirth if Hardware or Program already installed
    (testing "Installing Hardware before does prevent discount"
      (do-game
        (new-game {:runner {:deck ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"]}}
                  {:start-as :runner})
        (play-from-hand state :runner "Clone Chip")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner kate)
        (is (= kate (get-in (get-runner) [:identity :title])) "Rebirthed into Kate")
        (is (changes-credits (get-runner) -1
                             (play-from-hand state :runner "Akamatsu Mem Chip"))
            "Discount not applied for 2nd install")))
    (testing "Installing Resource before does not prevent discount"
      (do-game
        (new-game {:runner {:deck ["Akamatsu Mem Chip" "Rebirth" "Same Old Thing"]}}
                  {:start-as :runner})
        (play-from-hand state :runner "Same Old Thing")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner kate)
        (is (= kate (get-in (get-runner) [:identity :title])) "Rebirthed into Kate")
        (is (changes-credits (get-runner) 0
                             (play-from-hand state :runner "Akamatsu Mem Chip"))
            "Discount is applied for 2nd install (since it is the first Hardware / Program)"))))
  (testing "Rebirth into Reina twice"
    ;; Rebirth - Reina does not increase rez cost after rebirth if Ice already rezzed
    (testing "Rezzing Ice before does prevent cost"
      (do-game
        (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                   :runner {:id whizzard
                            :deck ["Rebirth"]}})
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Ice Wall" "R&D")
        (take-credits state :corp)
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-ice state :hq 0)))
            "Only pay 1 to rez ice wall when against Whizzard")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner reina)
        (is (= reina (get-in (get-runner) [:identity :title])) "Rebirthed into Reina")
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-ice state :rd 0)))
            "Additional cost from Reina not applied for 2nd ice rez")))
    (testing "Rezzing Asset before does not prevent additional cost"
      (do-game
        (new-game {:corp {:deck ["Ice Wall" "Mark Yale"]}
                   :runner {:id whizzard
                            :deck ["Rebirth"]}})
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Mark Yale" "New remote")
        (take-credits state :corp)
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-content state :remote1 0)))
            "Only pay 1 to rez Mark Yale")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner reina)
        (is (= reina (get-in (get-runner) [:identity :title])) "Rebirthed into Reina")
        (is (changes-credits (get-corp) -2
                             (core/rez state :corp (get-ice state :hq 0)))
            "Additional cost from Reina applied for 1st ice rez")))))
