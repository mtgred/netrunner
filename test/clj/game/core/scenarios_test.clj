(ns game.core.scenarios-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest tread-lightly-vovo-combine-well
  (do-game
    (new-game {:corp {:hand ["Tithe" "Vovô Ozetti"]}
               :runner {:hand ["Tread Lightly"]}})
    (play-from-hand state :corp "Tithe" "HQ")
    (play-from-hand state :corp "Vovô Ozetti" "HQ")
    (rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (click-prompt state :corp "No")
    (play-from-hand state :runner "Tread Lightly")
    (click-prompt state :runner "HQ")
    (is (changed? [(:credit (get-corp)) -2]
          (rez state :corp (get-ice state :hq 0)))
        "Spent 2 credits to rez tithe: (1 - 2 + 3) :: 2")))

(deftest hernando-cortez-vovo-combine-not-well
  (do-game
    (new-game {:corp {:hand ["Tithe" "Vovô Ozetti"]
                      :credits 20}
               :runner {:hand ["Hernando Cortez"]}})
    (play-from-hand state :corp "Tithe" "HQ")
    (play-from-hand state :corp "Vovô Ozetti" "HQ")
    (rez state :corp (get-content state :hq 0))
    (take-credits state :corp)
    (click-prompt state :corp "No")
    (play-from-hand state :runner "Hernando Cortez")
    (run-on state :hq)
    (is (changed? [(:credit (get-corp)) -2]
          (rez state :corp (get-ice state :hq 0)))
        "Spent 2 credits to rez tithe: (1 - 2 :: 0) + 2 :: 2")))

(deftest masterwork-overinstall-boomerang-complex-case
  (testing "for issue #7303"
    (dotimes [order 2]
      (do-game
        (new-game {:corp {:hand ["Tree Line" "Winchester" "Surveyor"]
                          :id "Weyland Consortium: Building a Better World"
                          :credits 20}
                   :runner {:hand ["The Class Act" "Masterwork (v37)" (qty "Boomerang" 2)]
                            :id "Zahya Sadeghi: Versatile Smuggler"
                            :credits 20
                            :deck [(qty "Easy Mark" 10)]}})
        (play-from-hand state :corp "Tree Line" "R&D")
        (play-from-hand state :corp "Winchester" "HQ")
        (play-from-hand state :corp "Surveyor" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (rez state :corp (get-ice state :rd 0))
        (take-credits state :corp)
        (play-from-hand state :runner "Masterwork (v37)")
        (play-from-hand state :runner "Boomerang")
        (click-card state :runner "Surveyor")
        (play-from-hand state :runner "The Class Act")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state "R&D")
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Boomerang" (:hand (get-runner))))
        (if (= order 0)
          (do (click-prompt state :runner "Boomerang")
              (click-card state :runner "Tree Line")
              (click-card state :runner (find-card "Easy Mark" (:set-aside (get-runner)))))
          (do (click-prompt state :runner "Masterwork (v37)")
              (click-card state :runner (find-card "Easy Mark" (:set-aside (get-runner))))
              (click-card state :runner "Tree Line")))
        (is (no-prompt? state :corp) "No lingering prompt")
        (is (no-prompt? state :runner) "No lingering prompt")
        (is (= 1 (count (:discard (get-runner)))))))))

(deftest masterwork-overinstall-boomerang-complex-case-full-game
  ;; I never want to write a unit test this long again
  (testing "for issue #7662"
    (do-game
      (new-game {:corp {:hand [(qty "Tree Line" 2) "Rashida Jaheem"
                               "Artificial Cryptocrash"
                               "Magnet" "Subliminal Messaging"]
                        :deck ["Subliminal Messaging"
                               "Business As Usual"
                               "Mestnichestvo"
                               "Tomorrow's Headline"
                               "Tree Line"
                               "Ubiquitous Vig"
                               "Rashida Jaheem"
                               (qty "Logjam" 3)
                               (qty "Offworld Office" 3)
                               (qty "NGO Front" 2)
                               "Artificial Cryptocrash"
                               (qty "Federal Fundraising" 2)
                               "Spin Doctor"
                               (qty "Vladisibirsk City Grid" 2)
                               "Hedge Fund"]
                        :identity "Pravdivost Consulting: Political Solutions"}
                 :runner {:hand ["Paladin Poemu" "The Twinning" "Dr. Nuka Vrolyck"
                                 "Dirty Laundry" "Jailbreak"]
                          :deck [(qty "Pinhole Threading" 2)
                                 (qty "Cezve" 2)
                                 "Dirty Laundry"
                                 (qty "Miss Bones" 2)
                                 (qty "The Class Act" 2)
                                 "Carmen"
                                 (qty "Boomerang" 2)
                                 "Diversion of Funds"
                                 "Inside Job"
                                 (qty "Bravado" 2)
                                 "Aumakua"
                                 (qty "Sure Gamble" 2)
                                 "Hermes"
                                 "Mutual Favor"
                                 "WAKE Implant v2A-JRJ"]
                          :identity "Zahya Sadeghi: Versatile Smuggler"}})
      ;; Corp Turn 1
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (play-from-hand state :corp "Tree Line" "HQ")
      (stack-deck state :corp
                  ["Subliminal Messaging" "Business As Usual" "Mestnichestvo"
                   "Artificial Cryptocrash" "Hedge Fund" "Tomorrow's Headline"
                   "Logjam" "NGO Front" "Federal Fundraising" "Logjam" "Offworld Office"
                   "Rashida Jaheem" "Offworld Office" "Vladisibirsk City Grid"
                   "Vladisibirsk City Grid" "Spin Doctor" "Federal Fundraising"
                   "Offworld Office" "Tree Line"
                   "NGO Front" "Ubiquitous Vig"])
      (click-credit state :corp)
      ;; Hand is: Magnet, Tree Line, Cryptocrash
      (is-hand? state :corp ["Tree Line" "Artificial Cryptocrash" "Magnet"])
      (is (= 7 (:credit (get-corp))) "Corp Turn 1: 3 in hand, 7 credits")
      (end-turn state :corp)
      (start-turn state :runner)
      ;; Runner Turn 1
      (stack-deck state :runner
                  ["Pinhole Threading" "Dirty Laundry" "Cezve" "Miss Bones" "Miss Bones"
                   "The Class Act" "Carmen" "The Class Act" "Pinhole Threading"
                   "Boomerang" "Diversion of Funds" "Inside Job" "Bravado"
                   "Aumakua" "Mutual Favor" "Sure Gamble" "Bravado" "Cezve"
                   "WAKE Implant v2A-JRJ" "Boomerang" "Hermes" "Sure Gamble"])
      (play-from-hand state :runner "Jailbreak")
      (click-prompt state :runner "R&D")
      (run-continue-until state :success)
      (click-card state :corp (get-ice state :hq 0))
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (run-empty-server state :remote1)
      (do-trash-prompt state 1)
      (play-from-hand state :runner "Paladin Poemu")
      (play-from-hand state :runner "Dr. Nuka Vrolyck")
      ;; Hand is: The Twinning, Dirty Laundry, Pinhole Threading
      (is-hand? state :runner ["The Twinning" "Dirty Laundry" "Pinhole Threading"])
      (is (= 4 (:credit (get-runner))) "Runner Turn 1: 3 in hand, 4 credits")
      (end-turn state :runner)
      ;; Corp Turn 2
      (start-turn state :corp)
      (click-draw state :corp)
      (play-from-hand state :corp "Subliminal Messaging")
      (click-draw state :corp)
      (play-from-hand state :corp "Tree Line" "R&D")
      ;; Hand is: Artificial Cryptocrash, Magnet, Business As Usual, Mestnichestvo
      (is-hand? state :corp
                ["Artificial Cryptocrash" "Magnet" "Business As Usual" "Mestnichestvo"])
      (is (= 8 (:credit (get-corp))) "Corp Turn 2: 4 in hand, 8 credits")
      (end-turn state :corp)
      ;; Runner Turn 2
      (start-turn state :runner)
      (card-ability state :runner (get-resource state 1) 0)
      (play-from-hand state :runner "Dirty Laundry")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (run-continue state)
      (click-card state :corp (get-ice state :rd 0))
      (play-from-hand state :runner "The Twinning")
      (click-prompt state :runner "Done")
      (play-from-hand state :runner "Cezve")
      (click-card state :runner "Paladin Poemu")
      ;; Hand is: Dirty Laundry, Pinhole Threading, Miss Bones
      (is-hand? state :runner ["Dirty Laundry" "Pinhole Threading" "Miss Bones"])
      (is (= 3 (:credit (get-runner))) "Runner Turn 2: 3 in hand, 3 credits")
      (end-turn state :runner)
      ;; Corp Turn 3
      (start-turn state :corp)
      (click-draw state :corp)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Mestnichestvo" "New remote")
      (end-turn state :corp)
      ;; Hand is: 2x Cryptocrash, Magnet, Business - 12 creds
      (is-hand? state :corp ["Artificial Cryptocrash" "Artificial Cryptocrash"
                             "Magnet" "Business As Usual"])
      (is (= 12 (:credit (get-corp))) "Corp Turn 3: 12 creds")
      ;; Runner Turn 3
      (start-turn state :runner)
      (card-ability state :runner (get-resource state 1) 0)
      (play-from-hand state :runner "Dirty Laundry")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (run-continue state)
      (click-card state :corp "Mestnichestvo")
      (play-from-hand state :runner "The Class Act")
      (click-credit state :runner)
      (end-turn state :runner)
      ;; Hand is: 2x pinhole, 2x bones, carmen, class act, boomerang, diversion - 3 creds
      (is (= 3 (:credit (get-runner))) "Runner Turn 3: 3 creds")
      (is-hand? state :runner ["Pinhole Threading" "Pinhole Threading" "Carmen"
                               "Miss Bones" "Miss Bones" "The Class Act" "Boomerang"
                               "Diversion of Funds"])
      ;; Corp Turn 4
      (start-turn state :corp)
      (play-from-hand state :corp "Tomorrow's Headline" "Server 2")
      (click-credit state :corp)
      (click-credit state :corp)
      (is-hand? state :corp ["Artificial Cryptocrash" "Artificial Cryptocrash"
                             "Magnet" "Business As Usual"])
      (is (= 14 (:credit (get-corp))) "Corp Turn 4: 14 creds")
      (end-turn state :corp)
      ;; Runner Turn 4
      (start-turn state :runner)
      (play-from-hand state :runner "Boomerang")
      (click-card state :runner "Paladin Poemu")
      (click-card state :runner "Paladin Poemu")
      (click-card state :runner (get-ice state :hq 0))
      (play-from-hand state :runner "Diversion of Funds")
      (run-continue-until state :success)
      (click-card state :corp "Tomorrow's Headline")
      (click-prompt state :runner "Diversion of Funds")
      (run-on state :hq)
      (core/move state :corp
                 (find-card "Artificial Cryptocrash" (:hand (get-corp))) :discard)
      (run-continue-until state :success)
      (click-prompt state :runner "2")
      (dotimes [_ 3]
        (if (= ["No action"] (prompt-buttons :runner))
          (click-prompt state :runner "No action")
          (click-prompt state :runner "Steal")))
      (click-prompt state :runner "Yes")
      (core/move state :corp
                 (find-card "Artificial Cryptocrash" (:discard (get-corp))) :hand)
      (end-turn state :runner)
      (click-card state :runner (find-card "Miss Bones" (:hand (get-runner))))
      (is (= 10 (:credit (get-runner))) "Runner Turn 4: 10 creds")
      (is-hand? state :runner ["Pinhole Threading" "Pinhole Threading" "Carmen"
                               "Miss Bones" "The Class Act"])
      ;; Corp Turn 5
      (start-turn state :corp)
      (trash state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Magnet" "HQ")
      (dotimes [_ 2]
        (click-advance state :corp (get-content state :remote2 0)))
      (score state :corp (get-content state :remote2 0))
      (end-turn state :corp)
      (is (= 7 (:credit (get-corp))) "Corp Turn 5: 7 creds")
      (is-hand? state :corp ["Business As Usual" "Artificial Cryptocrash" "Logjam"])
      ;; Runner Turn 5
      (start-turn state :runner)
      (click-draw state :runner)
      (click-card state :runner (find-card "Bravado" (:set-aside (get-runner))))
      (remove-tag state :runner)
      (play-from-hand state :runner "Carmen")
      (dotimes [_ 2] (click-card state :runner "Paladin Poemu"))
      (play-from-hand state :runner "The Class Act")
      (end-turn state :runner)
      (is (= 1 (:credit (get-runner))) "Runner Turn 5: 1 cred")
      (is-hand? state :runner ["Pinhole Threading" "Pinhole Threading" "Miss Bones"
                               "Inside Job" "Aumakua" "Mutual Favor" "Sure Gamble"
                               "Bravado"])
      ;; Corp Turn 6
      (start-turn state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Artificial Cryptocrash" "Server 2")
      (play-from-hand state :corp "NGO Front" "New remote")
      (play-from-hand state :corp "Business As Usual")
      (click-prompt state :corp "Place 1 advancement counter on up to two cards you can advance")
      (click-card state :corp (get-content state :remote2 0))
      (click-card state :corp (get-content state :remote3 0))
      (end-turn state :corp)
      (is (= 8 (:credit (get-corp))) "Corp Turn 6: 8 creds")
      (is-hand? state :corp ["Logjam" "Subliminal Messaging"])
      ;; Runner Turn 6
      (start-turn state :runner)
      (click-credit state :runner)
      (click-credit state :runner)
      (play-from-hand state :runner "Bravado")
      (click-prompt state :runner "Server 2")
      (run-continue state)
      (rez state :corp (get-ice state :remote2 0))
      (run-continue state :encounter-ice)
      (click-prompt state :corp "No")
      (fire-subs state (get-ice state :remote2 0))
      (play-from-hand state :runner "Pinhole Threading")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (run-continue state)
      (click-card state :corp (get-content state :remote2 0))
      (click-card state :runner (get-content state :remote3 0))
      (do-trash-prompt state 1)
      (click-card state :runner (get-program state 0))
      (end-turn state :runner)
      (click-card state :runner (find-card "Miss Bones" (:hand (get-runner))))
      (is (= 5 (:credit (get-runner))) "Runner Turn 6: 5 creds")
      (is-hand? state :runner ["Pinhole Threading" "Inside Job" "Aumakua" "Mutual Favor"
                               "Sure Gamble"])
      ;; Corp Turn 7
      (start-turn state :corp)
      (dotimes [_ 2]
        (click-advance state :corp (get-content state :remote2 0)))
      (score state :corp (get-content state :remote2 0))
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Federal Fundraising" "New remote")
      (end-turn state :corp)
      (is (= 2 (:credit (get-corp))) "Corp Turn 7: 2 creds")
      (is-hand? state :corp ["Logjam"])
      ;; Runner Turn 7
      (start-turn state :runner)
      (dotimes [_ 2] (click-credit state :runner))
      (run-on state :archives)
      (run-continue state :success)
      (click-card state :corp (get-ice state :rd 0))
      (click-credit state :runner)
      (is (= 3 (:credit (get-runner))) "Runner Turn 7: 3 creds")
      (is-hand? state :runner ["Pinhole Threading" "Inside Job" "Aumakua" "Mutual Favor"
                               "Sure Gamble"])
      (end-turn state :runner)
      ;; Corp Turn 7
      (rez state :corp (get-content state :remote4 0))
      (start-turn state :corp)
      (end-phase-12 state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Logjam")
      (click-prompt state :corp "Rashida Jaheem")
      (click-prompt state :corp "Offworld Office")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Yes")
      (play-from-hand state :corp "Offworld Office" "Server 2")
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (click-credit state :corp)
      (end-turn state :corp)
      (is (= 3 (:credit (get-corp))) "Corp Turn 8: 3 creds")
      (is-hand? state :corp ["Logjam"])
      ;; Runner Turn 8
      (start-turn state :runner)
      (dotimes [_ 2] (click-credit state :runner))
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Aumakua")
      (dotimes [_ 3] (click-card state :runner "Paladin Poemu"))
      (is (= 9 (:credit (get-runner))) "Runner Turn 8: 9 creds")
      (is-hand? state :runner ["Pinhole Threading" "Inside Job" "Mutual Favor"])
      (end-turn state :runner)
      ;; Corp Turn 9
      (rez state :corp (get-content state :remote5 0))
      (start-turn state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Yes")
      (end-phase-12 state :corp)
      (click-prompt state :corp "Rashida Jaheem")
      (click-prompt state :corp "Yes")
      (is-hand? state :corp ["Logjam" "Logjam"
                             "Subliminal Messaging" "Subliminal Messaging"
                             "Offworld Office" "Vladisibirsk City Grid"])
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Federal Fundraising")
      (click-prompt state :corp "Vladisibirsk City Grid")
      (click-prompt state :corp "Spin Doctor")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "Yes")
      (play-from-hand state :corp "Spin Doctor" "New remote")
      (play-from-hand state :corp "Vladisibirsk City Grid" "Server 2")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Vladisibirsk City Grid" "Server 4")
      (end-turn state :corp)
      (is (= 7 (:credit (get-corp))) "Corp Turn 9: 7 creds")
      (is-hand? state :corp ["Logjam" "Logjam" "Subliminal Messaging" "Offworld Office"])
      ;; Runner Turn 9
      (start-turn state :runner)
      (click-draw state :runner)
      (click-card state :runner "WAKE Implant v2A-JRJ")
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "Server 2")
      (run-continue-until state :success)
      (click-card state :corp (get-content state :remote4 1))
      (click-card state :runner (get-content state :remote2 1))
      (do-trash-prompt state 4)
      (click-prompt state :runner "Steal")
      (click-draw state :runner)
      (play-from-hand state :runner "Cezve")
      (dotimes [_ 2] (click-card state :runner "Paladin Poemu"))
      (end-turn state :runner)
      (is (= 3 (:credit (get-runner))) "Runner Turn 9: 3 creds")
      (is-hand? state :runner ["Pinhole Threading" "Boomerang" "Mutual Favor"])
      ;; Corp Turn 10
      (rez state :corp (get-content state :remote6 0))
      (start-turn state :corp)
      (end-phase-12 state :corp)
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Ubiquitous Vig")
      (click-prompt state :corp "NGO Front")
      (click-prompt state :corp "Tree Line")
      (click-prompt state :corp "Done")
      (click-prompt state :corp "No")
      (play-from-hand state :corp "Tree Line" "Server 4")
      (play-from-hand state :corp "Subliminal Messaging")
      (play-from-hand state :corp "Offworld Office" "Server 2")
      (play-from-hand state :corp "Logjam" "Archives")
      (end-turn state :corp)
      (is (= 8 (:credit (get-corp))) "Corp Turn 9: 7 creds")
      (is-hand? state :corp ["Logjam" "Offworld Office" "Federal Fundraising"])
      ;; Runner turn 10
      (start-turn state :runner)
      (click-draw state :runner)
      (click-card state :runner "Hermes")
      (play-from-hand state :runner "Boomerang")
      ;; here is where the bug occurs - if this doesn't throw an error, we're good
      (click-card state :runner "Paladin Poemu")
      (click-card state :runner (get-ice state :remote2 0)))))

(deftest maxx-annicam-buffer-drive-one-card-in-stack
  (testing "for issue #4966"
    (do-game
      (new-game {:runner {:hand ["Labor Rights" "Buffer Drive" "Aniccam"]
                          :discard ["Hacktivist Meeting" "Sure Gamble"]
                          :credits 10
                          :id "MaxX: Maximum Punk Rock"}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (play-from-hand state :runner "Aniccam")
      (play-from-hand state :runner "Labor Rights")
      (click-card state :runner "Hacktivist Meeting")
      (click-card state :runner "Sure Gamble")
      (take-credits state :runner)
      (let [in-hand (:title (first (:hand (get-runner))))
            in-deck (:title (first (:deck (get-runner))))]
        (take-credits state :corp)
        (click-prompt state :runner in-deck)
        (is (= [in-hand in-deck] (map :title (:hand (get-runner)))) "Aniccam drew the bottomed card")))))

(deftest degree-mill-cvs
  (testing "for issue #4515"
    (do-game
      (new-game {:corp {:hand ["Degree Mill"]
                        :discard ["Cyberdex Virus Suite"]}
                 :runner {:hand ["Aumakua" (qty "Clone Chip" 2)]
                          :credits 10}})
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Aumakua")
      (play-from-hand state :runner "Clone Chip")
      (play-from-hand state :runner "Clone Chip")
      (run-empty-server state :rd)
      (run-empty-server state :rd)
      (is (= 2 (get-counters (get-program state 0) :virus)) "Aumakua has 2 virus counters")
      (run-empty-server state :archives)
      (click-prompt state :corp "Yes")
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua has 1 virus counter after purge and no trash")
      (is (not (:run @state)) "Run has ended")
      (run-empty-server state :rd)
      (run-empty-server state :rd)
      (trash-from-hand state :corp "Degree Mill")
      (run-empty-server state :archives)
      (click-prompt state :runner "Degree Mill")
      (click-prompt state :runner "Pay to steal")
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner (get-hardware state 1))
      (click-prompt state :corp "Yes")
      (is (zero? (get-counters (get-program state 0) :virus)) "Aumakua has 0 virus counter after purge and steal")
      (is (not (:run @state)) "Run has ended"))))

(deftest minigame-prevent-netdmg-resourcetrash
  (testing "Mini-game testing prevention of net damage and resource trashing, with hosted Fall Guy"
    (do-game
      (new-game {:corp {:deck ["Neural EMP" (qty "Hedge Fund" 3) "SEA Source"]}
                 :runner {:deck ["Fall Guy" "Off-Campus Apartment" "Net Shield"
                                 "Wireless Net Pavilion" "Sure Gamble"]}})
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp 1)
      (is (= 14 (:credit (get-corp))))
      (core/gain state :runner :click 2)
      (run-empty-server state "Archives") ; enable Corp play of Neural and SEA next turn
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Off-Campus Apartment")
      (play-from-hand state :runner "Wireless Net Pavilion")
      (play-from-hand state :runner "Net Shield")
      (let [apt (get-resource state 0)]
        (play-from-hand state :runner "Fall Guy")
        (click-prompt state :runner (:title apt))
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))))
        (play-from-hand state :corp "Neural EMP")
        (let [fg (first (:hosted (refresh apt)))]
          (click-prompt state :runner "Net Shield")
          (is (= 5 (:credit (get-runner))) "Runner paid 1c to survive Neural EMP")
          (play-from-hand state :corp "SEA Source")
          (click-prompt state :corp "3") ; boost trace to 6
          (click-prompt state :runner "0")
          (is (= 1 (count-tags state)) "Runner took tag from SEA Source")
          (is (= 7 (:credit (get-corp))))
          (trash-resource state)
          (click-card state :corp "Off-Campus Apartment")
          (is (= 3 (:credit (get-corp))) "WNP increased cost to trash a resource by 2")
          (click-prompt state :runner "Fall Guy") ;; Trash Fall Guy to save the Apartment!
          (is (= (:title (get-resource state 0)) "Off-Campus Apartment")
              "Apartment still standing")
          (is (= (:title (last (:discard (get-runner)))) "Fall Guy") "Fall Guy trashed"))))))

(deftest hb-glacier
  (testing "HB Glacier econ and server protection with upgrades - Ash, Caprice, Breaker Bay Grid, positional ice strength boost"
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Engineering the Future"
                        :deck ["Adonis Campaign"
                               "Global Food Initiative"
                               "Breaker Bay Grid"
                               "Caprice Nisei"
                               "Ash 2X3ZB9CY"
                               "Turing"
                               "Hedge Fund"]}
                 :runner {:deck ["Desperado"
                                 "Dirty Laundry"
                                 "Emergency Shutdown"
                                 "Lamprey"
                                 "Data Folding"
                                 "Career Fair"]}})
      (draw state :corp 1)
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (is (= 10 (:credit (get-corp))) "HB:EtF ability paid 1 credit")
      (play-from-hand state :corp "Breaker Bay Grid" "Server 1")
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (let [adon (get-content state :remote1 0)
            bbg (get-content state :remote1 1)
            ash (get-content state :hq 0)]
        (rez state :corp bbg)
        (rez state :corp adon)
        (is (= 10 (:credit (get-corp))) "Breaker Bay Grid allowed rez of Adonis for free")
        (take-credits state :corp)
        (draw state :runner 1)
        (play-from-hand state :runner "Career Fair")
        (click-card state :runner (find-card "Data Folding" (:hand (get-runner))))
        (is (= 5 (:credit (get-runner))) "Data Folding installed for free by Career Fair")
        (play-from-hand state :runner "Lamprey")
        (play-from-hand state :runner "Desperado")
        (is (= 1 (:credit (get-runner))))
        (run-on state "HQ")
        (rez state :corp ash)
        (run-continue state)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (and (= 2 (:credit (get-runner))) (= 7 (:credit (get-corp))))
            "Desperado paid 1 to Runner, Lamprey took 1 from Corp")
        (click-prompt state :runner "No action") ; can't afford to trash Ash
        (take-credits state :runner)
        (play-from-hand state :corp "Caprice Nisei" "Server 1")
        (is (= 11 (:credit (get-corp))) "Gained 3 from Adonis and 1 from HB:EtF")
        (play-from-hand state :corp "Turing" "Server 1")
        (take-credits state :corp 1)
        (is (= 3 (:credit (get-runner))) "Gained 1 from Data Folding")
        (core/gain state :runner :click 2)
        (run-empty-server state "HQ")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (click-prompt state :runner "Pay 3 [Credits] to trash") ; trash Ash
        (is (and (= 1 (:credit (get-runner))) (= 11 (:credit (get-corp)))))
        (core/gain state :runner :credit 1)
        (play-from-hand state :runner "Dirty Laundry")
        (click-prompt state :runner "HQ")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "Steal")
        (is (= 2 (:agenda-point (get-runner))) "Stole Global Food Initiative")
        (is (and (= 6 (:credit (get-runner))) (= 10 (:credit (get-corp))))
            "Desperado plus Dirty Laundry, Lamprey took 1 from Corp")
        (run-on state "Server 1")
        (let [tur (get-ice state :remote1 0)
              cap (get-content state :remote1 2)]
          (rez state :corp tur)
          (run-continue state)
          (is (= 5 (get-strength (refresh tur))) "Turing +3 strength protecting a remote")
          (card-subroutine state :corp tur 0) ; end the run
          (click-prompt state :runner "End the run")
          (play-from-hand state :runner "Emergency Shutdown")
          (click-card state :runner tur)
          (is (not (:rezzed (refresh tur))) "Turing derezzed")
          (run-on state "Server 1") ; letting Runner in this time to use Caprice
          (rez state :corp cap)
          (run-continue state)
          ;; Caprice psi game started automatically
          (click-prompt state :corp "1 [Credits]")
          (click-prompt state :runner "2 [Credits]")
          (is (not (:run @state)) "Corp won Caprice psi game and ended the run"))))))

(deftest poetri-hidden-info-game
  ;; during a breach, if a known card (already seen) would move positions, it is known which card it is
  ;; ie, poetri installing a known card
  (dotimes [x 100]
    (let [unseen (atom #{"Hostile Takeover" "Ice Wall" "Tollbooth"})]
      (do-game
        (new-game {:corp {:id "Poétrï Luxury Brands: All the Rage"
                          :hand (vec @unseen)}
                   :runner {:hand ["Legwork"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Legwork")
        (run-continue-until state :success)
        ;; first access
        (let [cur (-> @state :runner :prompt first :card :title)]
          (swap! unseen disj cur)
          (if (= cur "Hostile Takeover")
            ;; install either one, doesn't matter
            (do (click-prompt state :runner "Steal")
                (click-prompts state :corp (first (shuffle @unseen)) "New remote")
                (is (second-last-log-contains? state "install ice protecting Server 1")
                    "New remote, no leaked info")
                (click-prompt state :runner "No action")
                (is (no-prompt? state :runner) "Saw 2 cards, access over"))
            ;; second access
            (do (click-prompt state :runner "No action")
                (let [cur (-> @state :runner :prompt first :card :title)]
                  (swap! unseen disj cur)
                  (if (= cur "Hostile Takeover")
                    (do (click-prompt state :runner "Steal")
                        (click-prompts state :corp (first @unseen) "New remote")
                        (is (last-log-contains? state "install ice protecting Server 1")
                            "New remote, no leaked info")
                        (is (no-prompt? state :runner) "Saw 2 cards, access over"))
                    (do (click-prompt state :runner "No action")
                        ;; third access -> this time we just install an ice, and it will be known
                        (let [target (first (shuffle ["Ice Wall" "Tollbooth"]))]
                          (click-prompt state :runner "Steal")
                          (click-prompts state :corp target "New remote")
                          (is (last-log-contains? state (str "to install " target " protecting Server 1")) "Exposed the info")
                          (is (no-prompt? state :runner) "Access over"))))))))))))

(deftest companions
  ;; Fencer Fueno, Mystic Maemi, Trickster Taka:
  ;; Gain 1c on start of turn or agenda steal
  (letfn [(companion-test [card]
            (do-game
              (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                                :hand ["Hostile Takeover"]}
                         :runner {:hand [card]}})
              (take-credits state :corp)
              (play-from-hand state :runner card)
              (let [cc (get-resource state 0)
                    counters (get-counters (refresh cc) :credit)]
                (is (zero? (get-counters (refresh cc) :credit)) "Companion starts with 0 credits")
                (run-empty-server state "HQ")
                (click-prompt state :runner "Steal")
                (is (= (inc counters) (get-counters (refresh cc) :credit)) "Companion gains 1c for stealing agenda")
                (run-empty-server state "Archives")
                (is (= (inc counters) (get-counters (refresh cc) :credit)) "Companion doesn't gain 1c when no agenda stolen"))))]
    (doall (map companion-test
                ["Fencer Fueno"
                 "Trickster Taka"
                 "Mystic Maemi"]))))

