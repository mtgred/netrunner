(ns game-test.cards.identities
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards)

(deftest FourHundredAndNineTeen-amoral-scammer
  ;; 419
  (testing "basic test: Amoral Scammer - expose first installed card unless corp pays 1 credit"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations"
                   ["PAD Campaign" "The Cleaners" (qty "Pup" 3) "Oaktown Renovation"])
        (make-deck "419: Amoral Scammer" []))
      (is (= 5 (:credit (get-corp))) "Starts with 5 credits")
      (play-from-hand state :corp "Pup" "HQ")
      (prompt-choice :runner "Yes")
      (prompt-choice :corp "Yes")
      (is (= 4 (:credit (get-corp))) "Pays 1 credit to not expose card")
      (play-from-hand state :corp "Pup" "HQ")
      (is (empty? (:prompt (get-runner))) "No option on second install")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Archives")
      (prompt-choice :runner "No")
      (is (empty? (:prompt (get-corp))) "No prompt if Runner chooses No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "The Cleaners" "New remote")
      (prompt-choice :runner "Yes")
      (prompt-choice :corp "No")
      (is (last-log-contains? state "exposes The Cleaners") "Installed card was exposed")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (empty? (:prompt (get-corp))) "Cannot expose faceup agendas")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/lose state :corp :credit (:credit (get-corp)))
      (is (= 0 (:credit (get-corp))) "Corp has no credits")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (prompt-choice :runner "Yes")
      (is (empty? (:prompt (get-corp))) "No prompt if Corp has no credits")
      (is (last-log-contains? state "exposes PAD Campaign") "Installed card was exposed")))
  (testing "Verify expose can be blocked"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" ["Underway Grid" "Pup"])
        (make-deck "419: Amoral Scammer" []))
      (play-from-hand state :corp "Underway Grid" "New remote")
      (prompt-choice :runner "No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Server 1")
      (prompt-choice :runner "Yes")
      (let [ug (get-in @state [:corp :servers :remote1 :content 0])]
        (core/rez state :corp ug)
        (prompt-choice :corp "No")
        (is (last-log-contains? state "uses Underway Grid to prevent 1 card from being exposed") "Exposure was prevented"))))
  (testing "Ixodidae shouldn't trigger off 419's ability"
    (do-game
      (new-game (default-corp ["PAD Campaign"])
                (make-deck "419: Amoral Scammer" ["Ixodidae"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Ixodidae")
      (take-credits state :runner)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (let [corp-credits (:credit (get-corp))
            runner-credits (:credit (get-runner))]
        (prompt-choice :runner "Yes")
        (prompt-choice :corp "Yes")
        (is (= 1 (- corp-credits (:credit (get-corp)))) "Should lose 1 credit from 419 ability")
        (is (= 0 (- runner-credits (:credit (get-runner)))) "Should not gain any credits from Ixodidae")))))

(deftest adam
  ;; Adam
  (testing "Allow runner to choose directives"
    (do-game
      (new-game
        (default-corp)
        (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
        {:dont-start-game true})
      (is (= 4 (count (get-in @state [:runner :play-area]))) "All directives are in the runner's play area")
      (is (= 0 (count (get-in @state [:runner :hand]))))
      (prompt-select :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (prompt-select :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (prompt-select :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (is (= 3 (count (get-in @state [:runner :rig :resource]))) "3 directives were installed")
      (is (= 0 (count (get-in @state [:runner :play-area]))) "The play area is empty")
      (let [nat (find-card "Neutralize All Threats" (get-in @state [:runner :rig :resource]))
            sf (find-card "Safety First" (get-in @state [:runner :rig :resource]))
            abr (find-card "Always Be Running" (get-in @state [:runner :rig :resource]))]
        (is (and nat sf abr) "The chosen directives were installed"))))
  (testing "Directives should not grant Pālanā credits"
    (do-game
      (new-game
        (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
        (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
        {:dont-start-game true})
      (prompt-select :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (prompt-select :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (prompt-select :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (prompt-choice :corp "Keep")
      (prompt-choice :runner "Keep")
      (core/start-turn state :corp nil)
      (is (= 5 (:credit (get-corp))) "Pālanā does not gain credit from Adam's starting Directives")))
  (testing "Neutralize All Threats interaction with advanceable traps"
    (do-game
      (new-game
        (default-corp [(qty "Cerebral Overwriter" 3)])
        (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
        {:dont-start-game true})
      (prompt-select :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (prompt-select :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (prompt-select :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (prompt-choice :corp "Keep")
      (prompt-choice :runner "Keep")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (advance state (get-content state :remote1 0) 2)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (prompt-choice :corp "Yes")
      (prompt-choice-partial :runner "Pay")
      (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "1 card in archives"))))

(deftest andromeda
  ;; Andromeda - 9 card starting hand, 1 link
  (do-game
    (new-game
      (default-corp)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 1 (:link (get-runner))) "1 link")
    (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-mulligan
  ;; Andromeda - 9 card starting hand after mulligan
  (do-game
    (new-game
      (default-corp)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)])
      {:mulligan :runner})
    (is (= 1 (:link (get-runner))) "1 link")
    (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-palana
  ;; Andromeda - should not grant Palana credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 5 (:credit (get-corp))) "Palana does not gain credit from Andromeda's starting hand")))

(deftest apex-facedown-console
  ;; Apex - Allow facedown install of a second console. Issue #1326
  (do-game
    (new-game
      (default-corp)
      (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2)]))
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (prompt-choice :runner "Done") ; no facedown install on turn 1
    (play-from-hand state :runner "Heartbeat")
    (is (= 1 (count (get-in @state [:runner :rig :hardware]))))
    (take-credits state :runner)
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (prompt-select :runner (find-card "Heartbeat" (:hand (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rig :facedown]))) "2nd console installed facedown")))

(deftest ayla
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game
      (default-corp)
      (make-deck "Ayla \"Bios\" Rahim: Simulant Specialist" ["Sure Gamble" "Desperado"
                                                             "Security Testing" "Bank Job"
                                                             "Heartbeat" "Eater"])
      {:dont-start-game true})
    (is (= 6 (count (get-in @state [:runner :play-area]))) "Deck cards are in play area")
    (is (= 0 (count (get-in @state [:runner :hand]))))
    (prompt-select :runner (find-card "Sure Gamble" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Desperado" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Bank Job" (get-in @state [:runner :play-area])))
    (prompt-select :runner (find-card "Eater" (get-in @state [:runner :play-area])))
    (is (= 4 (count (:hosted (:identity (get-runner))))) "4 cards in NVRAM")
    (is (= 0 (count (get-in @state [:runner :play-area]))) "The play area is empty")
    (prompt-choice :corp "Keep")
    (prompt-choice :runner "Keep")
    (take-credits state :corp)
    (is (= 2 (count (get-in @state [:runner :hand]))) "There are 2 cards in the runner's Grip")
    (card-ability state :runner (:identity (get-runner)) 0)
    (prompt-card :runner (find-card "Bank Job" (:hosted (:identity (get-runner)))))
    (is (= 3 (count (get-in @state [:runner :hand]))) "There are 3 cards in the runner's Grip")))

(deftest cerebral-imaging-max-hand-size
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Has 13 credits")
    (is (= 13 (core/hand-size state :corp)) "Max hand size is 13")))

(deftest chronos-protocol
  ;; Chronos Protocol - Choose Runner discard for first net damage of a turn
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" ["Pup" (qty "Neural EMP" 2)])
      (default-runner [(qty "Imp" 3)]))
    (play-from-hand state :corp "Pup" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [pup (get-ice state :hq 0)]
      (core/rez state :corp pup)
      (card-subroutine state :corp pup 0)
      (prompt-choice :corp "Yes")
      (let [imp (find-card "Imp" (:hand (get-runner)))]
        (prompt-choice :corp imp)
        (is (= 1 (count (:discard (get-runner)))))
        (card-subroutine state :corp pup 0)
        (is (empty? (:prompt (get-corp))) "No choice on second net damage")
        (is (= 2 (count (:discard (get-runner)))))
        (run-jack-out state)
        (take-credits state :runner)
        (core/move state :runner (find-card "Imp" (:discard (get-runner))) :hand)
        (play-from-hand state :corp "Neural EMP")
        (prompt-choice :corp "No")
        (is (= 2 (count (:discard (get-runner)))) "Damage dealt after declining ability")
        (play-from-hand state :corp "Neural EMP")
        (is (empty? (:prompt (get-corp))) "No choice after declining on first damage")
        (is (= 3 (count (:discard (get-runner)))))))))

(deftest chronos-protocol-obokata-protocol
  ;; Pay 4 net damage to steal.  Only 3 damage left after Chronos.  No trigger of damage prevent.
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Obokata Protocol" 5)])
              (default-runner [(qty "Sure Gamble" 3) "Inti" "Feedback Filter"]))
    (core/gain state :runner :credit 10)
    (play-from-hand state :corp "Obokata Protocol" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (run-empty-server state "Server 1")
    (prompt-choice-partial :runner "Pay")
    (prompt-choice :corp "Yes")
    (prompt-card :corp (find-card "Inti" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "Feedback Filter net damage prevention opportunity not given")
    (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 net damage")))

(deftest chronos-protocol-employee-strike
  ;; Chronos Protocol - Issue #1958 also affects Chronos Protocol
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" ["Pup"])
      (default-runner ["Employee Strike" (qty "Scrubbed" 3) "Sure Gamble"]))
    (play-from-hand state :corp "Pup" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Employee Strike")
    (run-on state :hq)
    (let [pup (get-ice state :hq 0)]
      (core/rez state :corp pup)
      (card-subroutine state :corp pup 0)
      (is (empty? (:prompt (get-corp))) "No choice because of Employee Strike")
      (card-subroutine state :corp pup 0)
      (is (= 2 (count (:discard (get-runner)))))
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Scrubbed")
      (run-on state :hq)
      (card-subroutine state :corp pup 0)
      (is (not (empty? (:prompt (get-corp)))) "Employee Strike out of play - Ability turned on correctly"))))

(deftest edward-kim
  ;; Edward Kim
  (testing "Trash first operation accessed each turn, but not if first one was in Archives"
    (do-game
      (new-game
        (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 2) "PAD Campaign"])
        (make-deck "Edward Kim: Humanity's Hammer" ["Eater" (qty "Sure Gamble" 2)]))
      (play-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "PAD Campaign")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "No operation trashed from HQ; accessed one in Archives first")
      (take-credits state :runner)
      (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :hand)
      (is (= 1 (count (:discard (get-corp)))))
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
      (take-credits state :runner)
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (play-from-hand state :runner "Eater")
      (let [eater (get-in @state [:runner :rig :program 0])]
        (run-on state "Archives")
        (card-ability state :runner eater 0) ; pretend to break a sub so no cards in Archives will be accessed
        (run-successful state)
        (is (= 3 (count (:discard (get-corp)))))
        (run-empty-server state "HQ")
        (is (= 4 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))
  (testing "Do not trigger maw on first Operation access (due to trash)"
    (do-game
      (new-game
        (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 2)])
        (make-deck "Edward Kim: Humanity's Hammer" ["Maw" (qty "Sure Gamble" 2)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Maw")
      (is (= 0 (count (:discard (get-corp)))) "No cards in Archives")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Only one card trashed from HQ, by Ed Kim")
      (run-empty-server state "HQ")
      (prompt-choice :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "One more card trashed from HQ, by Maw"))))

(deftest exile
  ;; Exile
  (testing "Simultaneous-resolution prompt shown for interaction with Customized Secretary"
    (do-game
      (new-game
        (default-corp)
        (make-deck "Exile: Streethawk" [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                        (qty "Sure Gamble" 3)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Customized Secretary" "Clone Chip"])
      (trash-from-hand state :runner "Customized Secretary")
      (play-from-hand state :runner "Clone Chip")
      (card-ability state :runner (get-hardware state 0) 0)
      (prompt-select :runner (find-card "Customized Secretary" (:discard (get-runner))))
      ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
      (prompt-choice :runner "Exile: Streethawk")
      (is (= 1 (count (:hand (get-runner)))) "Exile drew a card"))))

(deftest freedom-khumalo
  ;; Freedom Khumalo - Can spend virus counters from other cards to trash accessed cards with play/rez costs
  (testing "Only works with Assets, ICE, Operations, and Upgrades"
    (letfn [(fk-test [card]
              (do-game
                (new-game (default-corp [(qty card 1)])
                          (make-deck "Freedom Khumalo: Crypto-Anarchist"
                                     ["Cache"]))
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (prompt-choice-partial :runner "Freedom")
                (prompt-select :runner (get-program state 0))
                (prompt-select :runner (get-program state 0))
                (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now")))]
      (doall (map fk-test
                  ["Dedicated Response Team"
                   "Consulting Visit"
                   "Builder"
                   "Research Station"]))))
  (testing "Triggers when play/rez cost less than or equal to number of available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game (default-corp [(qty card 1)])
                          (make-deck "Freedom Khumalo: Crypto-Anarchist"
                                     ["Cache"]))
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (let [cost (->> (get-corp) :hand first :cost)]
                  (prompt-choice-partial :runner "Freedom")
                  (when (< 0 cost)
                    (dotimes [_ cost]
                      (prompt-select :runner (get-program state 0)))))
                (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now")))]
      (doall (map fk-test
                  ["Beanstalk Royalties"
                   "Aggressive Negotiation"
                   "Consulting Visit"
                   "Door to Door"]))))
  (testing "Doesn't trigger when there aren't enough available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game (default-corp [(qty card 1)])
                          (make-deck "Freedom Khumalo: Crypto-Anarchist"
                                     ["Cache"]))
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (is (= 1 (->> @state :runner :prompt first :choices count)) "Should only have 1 option")
                (is (= "No action" (->> @state :runner :prompt first :choices first)) "Only option should be 'No action'")))]
      (doall (map fk-test
                  ["Archer"
                   "Fire Wall"
                   "Colossus"
                   "Tyrant"]))))
  (testing "Can use multiple programs for virus counter payment"
    (do-game
      (new-game (default-corp ["Dedicated Response Team"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Cache" "Virus Breeding Ground"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Virus Breeding Ground")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (prompt-choice-partial :runner "Freedom")
      (prompt-select :runner (get-program state 0))
      (prompt-select :runner (get-resource state 0))
      (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now")))
  (testing "Can use viruses on hosted cards"
    (do-game
      (new-game (default-corp [(qty "Ice Wall" 2)])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Trypano"]))
      (play-from-hand state :corp "Ice Wall" "R&D")
      (let [iw (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (prompt-select :runner (refresh iw))
        (take-credits state :runner)
        (take-credits state :corp)
        (prompt-choice :runner "Yes")
        (run-empty-server state "HQ")
        (prompt-choice-partial :runner "Freedom")
        (prompt-select :runner (->> (refresh iw) :hosted first)))
      (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now")))
  (testing "Doesn't trigger when accessing an Agenda"
    (do-game
      (new-game (default-corp ["Hostile Takeover"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Cache"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= 1 (->> @state :runner :prompt first :choices count)) "Should only have 1 option")
      (is (= "Steal" (->> @state :runner :prompt first :choices first)) "Only option should be 'Steal'")))
  (testing "Shows multiple prompts when playing Imp"
    (do-game
      (new-game (default-corp ["Dedicated Response Team"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Sure Gamble" "Cache" "Imp"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Imp")
      (run-empty-server state "HQ")
      (is (= 4 (->> @state :runner :prompt first :choices count)) "Should have 4 options: Freedom, Imp, Trash, No action")))
  (testing "Should return to access prompts when Done is pressed"
    (do-game
      (new-game (default-corp ["Dedicated Response Team"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Cache"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= 3 (->> @state :runner :prompt first :choices count)) "Should have 3 prompts: Freedom, Trash, No action")
      (prompt-choice-partial :runner "Freedom")
      (prompt-select :runner (get-program state 0))
      (prompt-choice :runner "Done")
      (is (= 3 (->> @state :runner :prompt first :choices count)) "Should go back to access prompts")
      (prompt-choice-partial :runner "Freedom")
      (prompt-select :runner (get-program state 0))
      (prompt-select :runner (get-program state 0))
      (is (= 1 (count (:discard (get-corp)))) "Card should now be properly discarded")))
  (testing "Shouldn't grant additional accesses after trashing accessed card. #3423"
    (do-game
      (new-game (default-corp [(qty "Ice Wall" 10)])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Cache"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "R&D")
      (prompt-choice-partial :runner "Freedom")
      (prompt-select :runner (get-program state 0))
      (is (= 1 (count (:discard (get-corp)))) "Card should be discarded now")
      (is (not (:run @state)) "Run ended")))
  (testing "Shouldn't give Aumakua additional counters on trash. #3479"
    (do-game
      (new-game (default-corp [(qty "Ice Wall" 10)])
                (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache" "Aumakua"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state "R&D")
      (is (nil? (->> (get-program state 1) :counter :virus)) "Aumakuma shouldn't have any virus counters yet.")
      (prompt-choice-partial :runner "Freedom")
      (prompt-select :runner (get-program state 0))
      (is (= 1 (count (:discard (get-corp)))) "Ice Wall should be discarded now")
      (is (nil? (->> (get-program state 1) :counter :virus)) "Aumakua doesn't gain any virus counters from trash ability.")
      (is (not (:run @state)) "Run ended"))))

(deftest gabriel-santiago
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game
      (default-corp)
      (make-deck "Gabriel Santiago: Consummate Professional" ["Easy Mark"]))
    (take-credits state :corp)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-runner))) "No credits gained")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "No credits gained")))

(deftest gagarin
  ;; Gagarin - pay 1c to access each card in remote
  (do-game
    (new-game
      (make-deck "Gagarin Deep Space: Expanding the Horizon" ["PAD Campaign" "Caprice Nisei"])
      (default-runner))
    (core/lose state :runner :credit 4)
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (prompt-select :runner (get-content state :remote1 0))
    (is (= 0 (:credit (get-runner))) "Paid 1 credit to access")
    (prompt-choice :runner "No") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :remote1)
    (prompt-select :runner (get-content state :remote1 0))
    (prompt-choice :runner "No action") ; Could not afford message dismissed
    (is (empty? (:prompt (get-runner))) "Runner cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (prompt-choice :runner "No") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprice") "Accessed card name was logged")))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (default-runner))
    (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))

(deftest grndl-valencia
  ;; GRNDL vs Valencia - only 1 bad pub at start
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))

(deftest haarpsichord-studios
  ;; Haarpsichord Studios
  (testing "Prevent stealing more than 1 agenda per turn"
    (do-game
      (new-game
        (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
        (default-runner ["Gang Sign"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (run-empty-server state "HQ")
      (prompt-choice :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (run-empty-server state "HQ")
      (prompt-choice :runner "No action")
      (is (= 1 (:agenda-point (get-runner))) "Second steal of turn prevented")
      (take-credits state :runner)
      (play-from-hand state :corp "15 Minutes" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (prompt-choice :runner "Card from hand")
      (prompt-choice :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Steal prevention didn't carry over to Corp turn")))
  (testing "Interactions with Employee Strike. Issue #1313"
    (do-game
      (new-game
        (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
        (default-runner ["Employee Strike" "Scrubbed"]))
      (take-credits state :corp)
      (core/gain state :runner :click 5)
      (run-empty-server state "HQ")
      (prompt-choice :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (play-from-hand state :runner "Employee Strike")
      (run-empty-server state "HQ")
      (prompt-choice :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Second steal not prevented")
      (play-from-hand state :runner "Scrubbed")
      (run-empty-server state "HQ")
      (prompt-choice :runner "No action")
      (is (= 2 (:agenda-point (get-runner))) "Third steal prevented"))))

(deftest haas-bioroid-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to rez after passing bioroid
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Architects of Tomorrow" [(qty "Eli 1.0" 2) "Pup"])
      (default-runner))
    (core/gain state :corp :credit 3)
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (play-from-hand state :corp "Pup" "Archives")
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (take-credits state :corp)
    (run-on state "Archives")
    (core/rez state :corp (get-ice state :archives 1))
    (run-continue state)
    (core/rez state :corp (get-ice state :archives 0))
    (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (prompt-select :corp (get-ice state :hq 0))
    (is (= 3 (:credit (get-corp))) "Corp not charged for Architects of Tomorrow rez of Eli 1.0")))

(deftest haas-bioroid-asa-group
  ;; Asa Group - don't allow installation of operations
  (do-game
    (new-game
      (make-deck "Asa Group: Security Through Vigilance" ["Pup" "BOOM!" "Urban Renewal"])
      (default-runner))
    (play-from-hand state :corp "Pup" "New remote")
    (prompt-select :corp (find-card "BOOM!" (:hand (get-corp))))
    (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
    (prompt-select :corp (find-card "Urban Renewal" (:hand (get-corp))))
    (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))

(deftest haas-bioroid-engineering-the-future-employee-strike
  ;; EtF - interaction with Employee Strike
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Engineering the Future" [(qty "Eli 1.0" 3) "Paywall Implementation"])
      (default-runner ["Employee Strike"]))
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits at turn end")
    (play-from-hand state :runner "Employee Strike")
    (take-credits state :runner)
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
    (play-from-hand state :corp "Paywall Implementation")
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (is (= 9 (:credit (get-corp))) "Corp gained 1cr from EtF")))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Stronger Together" ["Eli 1.0"])
      (default-runner))
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (core/rez state :corp eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest iain-stirling-credits
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game
      (default-corp ["Breaking News"])
      (make-deck "Iain Stirling: Retired Spook" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (take-credits state :corp)
      (is (= 1 (:agenda-point (get-corp))) "Corp gains 1 agenda point from Breaking News")
      (take-credits state :runner 1)
      (is (= 8 (:credit (get-runner))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-trash-cost
  ;; Industrial Genomics - Increase trash cost
  (do-game
    (new-game
      (make-deck "Industrial Genomics: Growing Solutions" [(qty "PAD Campaign" 3)
                                                           (qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (trash-from-hand state :corp "PAD Campaign")
    (trash-from-hand state :corp "PAD Campaign")
    (trash-from-hand state :corp "Hedge Fund")
    (trash-from-hand state :corp "Hedge Fund")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :corp pad)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 8 (core/trash-cost state :runner (refresh pad)))))))

(deftest jemison-astronautics
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (do-game
    (new-game
      (make-deck "Jemison Astronautics: Sacrifice. Audacity. Success." ["Enforcer 1.0" "Hostile Takeover"
                                                                        "Ice Wall" "Global Food Initiative"])
      (default-runner ["Data Dealer"]))
    (play-from-hand state :corp "Enforcer 1.0" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [enf (get-ice state :hq 0)
          iwall (get-ice state :rd 0)]
      (take-credits state :corp)
      (play-from-hand state :runner "Data Dealer")
      (run-empty-server state "Server 1")
      (prompt-choice :runner "Steal")
      (let [dd (get-resource state 0)]
        (card-ability state :runner dd 0)
        (prompt-select :runner (get-in (get-runner) [:scored 0]))
        (is (empty? (:prompt (get-corp))) "No Jemison prompt for Runner forfeit")
        (take-credits state :runner)
        (play-from-hand state :corp "Global Food Initiative" "New remote")
        (score-agenda state :corp (get-content state :remote2 0))
        (core/rez state :corp enf)
        (prompt-select :corp (get-in (get-corp) [:scored 0]))
        (prompt-select :corp iwall)
        (is (= 4 (:advance-counter (refresh iwall))) "Jemison placed 4 advancements")))))

(deftest jemison-24-intimidation
  ;; Test Jemison - 24/7 - Armed Intimidation combination
  ;; Expected result: 24/7 causes Forfeit, Jemison places counters, AI triggers
  (do-game
    (new-game
      (make-deck "Jemison Astronautics: Sacrifice. Audacity. Success."
                 ["Armed Intimidation" "Hostile Takeover"
                  "24/7 News Cycle" "Ice Wall"])
      (default-runner))
    (play-and-score state "Hostile Takeover")
    (is (= 1 (:agenda-point (get-corp))) "Corp has 1 agenda points from Hostile Takeover")
    (is (= 12 (:credit (get-corp))) "Corp has 12 credits after scoring Hostile Takeover with play-score")
    (play-and-score state "Armed Intimidation")
    (prompt-choice :runner "Take 2 tags")
    (is (= 3 (:agenda-point (get-corp))) "Corp has 3 agenda points from HT + Armed Intimidation")
    (is (= 2 (:tag (get-runner))) "Runner took 2 tags from AI")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (take-credits state :runner)

    (play-from-hand state :corp "24/7 News Cycle")
    (prompt-select :corp (get-scored state :corp 0))        ; select HT to forfeit

    (let [ice-wall (get-ice state :hq 0)]
      (prompt-select :corp ice-wall)                        ; The Jemison forfeit triggers
      (is (= 2 (:advance-counter (refresh ice-wall))) "Ice Wall has 2 advancement counters from HT forfeit"))

    (prompt-select :corp (get-scored state :corp 0))        ; select AI to trigger
    (prompt-choice :runner "Take 2 tags")                   ; First runner has prompt
    (is (= 4 (:tag (get-runner))) "Runner took 2 more tags from AI -- happens at the end of all the delayed-completion")))

(deftest jesminder-sareen-ability
  ;; Jesminder Sareen - avoid tags only during a run
  (do-game
    (new-game (default-corp ["SEA Source" "Data Raven"])
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Data Raven" "Archives")
    (take-credits state :corp)
    (let [dr (-> @state :corp :servers :archives :ices first)]
      (core/rez state :corp dr)
      (core/click-run state :runner {:server "Archives"})
      (card-ability state :corp dr 0)
      (is (= 0 (:tag (get-runner))) "Jesminder avoided first tag during the run")
      (card-ability state :corp dr 0)
      (is (= 1 (:tag (get-runner))) "Jesminder did not avoid the second tag during the run")
      (core/no-action state :corp nil)
      (core/continue state :runner nil)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (run-empty-server state "R&D") ; clear per-run buffer
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 2 (:tag (get-runner))) "Jesminder did not avoid the tag outside of a run"))))

(deftest jesminder-john-masanori
  ;; Jesminder Sareen - don't avoid John Masanori tag
  (do-game
    (new-game (default-corp)
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" ["John Masanori"]))
    (take-credits state :corp)
    (play-from-hand state :runner "John Masanori")
    (run-on state "HQ")
    (core/jack-out state :runner nil)
    (is (= 1 (:tag (get-runner))) "Jesminder did not avoid John Masanori tag")))

(deftest jinteki-biotech-brewery
  ;; Jinteki Biotech - Brewery net damage
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" ["Braintrust"])
      (default-runner)
      {:dont-start-turn true})
    (prompt-choice :corp "The Brewery")
    (core/start-turn state :corp nil)
    (card-ability state :corp (:identity (get-corp)) 1)
    (is (= 1 (count (:hand (get-runner)))) "Runner took 2 net damage from Brewery flip")))

(deftest jinteki-biotech-greenhouse
  ;; Jinteki Biotech - Greenhouse four advancement tokens
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" ["Braintrust"])
      (default-runner)
      {:dont-start-turn true})
    (prompt-choice :corp "The Greenhouse")
    (core/start-turn state :corp nil)
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (take-credits state :runner)
    (let [bt (get-content state :remote1 0)]
      (is (nil? (:advance-counter (refresh bt))) "No advancement counters on agenda")
      (card-ability state :corp (:identity (get-corp)) 1)
      (prompt-select :corp (refresh bt))
      (is (= 4 (:advance-counter (refresh bt))) "Four advancement counters on agenda"))))

(deftest jinteki-biotech-tank
  ;; Jinteki Biotech - Tank shuffle Archives into R&D
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Hedge Fund" 3)])
      (default-runner)
      {:dont-start-turn true})
    (prompt-choice :corp "The Tank")
    (core/start-turn state :corp nil)
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :runner)
    (is (= 3 (count (:discard (get-corp)))) "Archives started with 3 cards")
    (is (= 0 (count (:deck (get-corp)))) "R&D started empty")
    (card-ability state :corp (:identity (get-corp)) 1)
    (is (= 0 (count (:discard (get-corp)))) "Archives ended empty")
    (is (= 3 (count (:deck (get-corp)))) "R&D ended with 3 cards")))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Braintrust" 6)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Steal")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage from steal")))

(deftest jinteki-potential-unleashed
  ;; PU - when the runner takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game (make-deck "Jinteki: Potential Unleashed" ["Philotic Entanglement" "Neural EMP" (qty "Braintrust" 3)])
              (default-runner [(qty "Employee Strike" 10)]))
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (prompt-choice :runner "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Philotic Entanglement" "New remote")
    (score-agenda state :corp (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-runner)))))
    (play-from-hand state :corp "Neural EMP")
    (is (= 5 (count (:discard (get-runner)))))))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-runner))
    (play-from-hand state :corp "Mental Health Clinic" "New remote")
    (take-credits state :corp)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (run-empty-server state "HQ")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")))

(deftest jinteki-replicating-perfection-employee-strike
  ;; Replicating Perfection - interaction with Employee Strike. Issue #1313 and #1956.
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-runner ["Employee Strike" "Scrubbed"]))
    (play-from-hand state :corp "Mental Health Clinic" "New remote")
    (take-credits state :corp)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (play-from-hand state :runner "Employee Strike")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")
    (play-from-hand state :runner "Scrubbed")
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")))

(deftest kate-mac-mccaffrey-discount
  ;; Kate 'Mac' McCaffrey - Install discount
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" ["Magnum Opus"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

(deftest kate-mac-mccaffrey-no-discount
  ;; Kate 'Mac' McCaffrey - No discount for 0 cost
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         ["Magnum Opus"
                          "Self-modifying Code"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Magnum Opus")
    (is (= 0 (:credit (get-runner))) "No Kate discount on second program install")))

(deftest kate-mac-mccaffrey-discount-cant-afford
  ;; Kate 'Mac' McCaffrey - Can Only Afford With the Discount
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" ["Magnum Opus"]))
    (take-credits state :corp)
    (core/lose state :runner :credit 1)
    (is (= 4 (:credit (get-runner))))
    (play-from-hand state :runner "Magnum Opus")
    (is (= 1 (count (get-in @state [:runner :rig :program]))) "Magnum Opus installed")
    (is (= 0 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

(deftest ken-tenma-run-event-credit
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game (default-corp)
              (make-deck "Ken \"Express\" Tenma: Disappeared Clone" [(qty "Account Siphon" 2)]))
    (take-credits state :corp)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit for first Run event")
    (prompt-choice :runner "Replacement effect")
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 16 (:credit (get-runner))) "No credit gained for second Run event")))

(deftest khan-vs-caprice
  ;; Khan - proper order of events when vs. Caprice
  (do-game
    (new-game
      (default-corp ["Eli 1.0" "Caprice Nisei"])
      (make-deck "Khan: Savvy Skiptracer" ["Corroder"]))
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (play-from-hand state :corp "Caprice Nisei" "Archives")
    (core/rez state :corp (get-content state :archives 0))
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (is (and (empty? (:prompt (get-corp)))
             (= 1 (count (:prompt (get-runner))))
             (= "Khan: Savvy Skiptracer" (-> (get-runner) :prompt first :card :title)))
        "Only Khan prompt showing")
    (prompt-select :runner (first (:hand (get-runner))))
    (is (find-card "Corroder" (-> (get-runner) :rig :program)) "Corroder installed")
    (is (= 4 (:credit (get-runner))) "1cr discount from Khan")
    (is (= "Caprice Nisei" (-> (get-runner) :prompt first :card :title)) "Caprice prompt showing")
    (prompt-choice :runner "0 [Credits]")
    (prompt-choice :corp "1 [Credits]")
    (is (not (:run @state)) "Run ended")))

(deftest laramy-fisk-shards
  ;; Laramy Fisk - installing a Shard should still give option to force Corp draw.
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" ["Eden Shard"]))
    (starting-hand state :corp ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Eli 1.0" "Eli 1.0"])
    (take-credits state :corp)
    (run-on state "R&D")
    (core/no-action state :corp nil)
    ;; at Successful Run stage -- click Eden Shard to install
    (play-from-hand state :runner "Eden Shard")
    (is (= 5 (:credit (get-runner))) "Eden Shard install was free")
    (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
    (is (= "Identity" (-> (get-runner) :prompt first :card :type)) "Fisk prompt showing")
    (prompt-choice :runner "Yes")
    (is (not (:run @state)) "Run ended")
    (is (= 6 (count (:hand (get-corp)))) "Corp forced to draw")))

(deftest leela-gang-sign-complicated
  ;; Leela Patel - complicated interaction with mutiple Gang Sign
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" ["Project Atlas"
                                                                  "Hostile Takeover"
                                                                  "Geothermal Fracking"])
      (make-deck "Leela Patel: Trained Pragmatist" [(qty "Gang Sign" 2)]))
    (play-from-hand state :corp "Project Atlas" "New remote")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "Geothermal Fracking" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Gang Sign")
    (play-from-hand state :runner "Gang Sign")
    (take-credits state :runner)
    (score-agenda state :corp (get-content state :remote1 0))
    (prompt-choice :runner "Leela Patel: Trained Pragmatist")
    (prompt-select :runner (get-content state :remote2 0))
    (is (find-card "Hostile Takeover" (:hand (get-corp))) "Hostile Takeover returned to hand")
    (prompt-choice :runner "Gang Sign")
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-runner))) "Hostile Takeover stolen with Gang Sign")
    (prompt-select :runner (get-content state :remote3 0))
    (is (find-card "Geothermal Fracking" (:hand (get-corp))) "Geothermal Fracking returned to hand")
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-runner))) "Geothermal Fracking stolen with Gang Sign")
    (prompt-choice :runner "Done")))

(deftest leela-lingering-successful-run-prompt
  ;; Leela Patel - issues with lingering successful run prompt
  (do-game
    (new-game
      (make-deck "NBN: Making News" ["Breaking News" "SanSan City Grid"])
      (make-deck "Leela Patel: Trained Pragmatist" []))
    (starting-hand state :corp ["SanSan City Grid"])
    (play-from-hand state :corp "SanSan City Grid" "New remote")
    (take-credits state :corp)
    (run-empty-server state :rd)
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :remote1 0))
    (is (not (:run @state)) "Run is over")))

(deftest leela-upgrades
  ;; Leela Patel - upgrades returned to hand in the middle of a run do not break the run. Issue #2008.
  (do-game
    (new-game (default-corp [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) "Shock!"])
              (make-deck "Leela Patel: Trained Pragmatist" ["Sure Gamble"]))
    (starting-hand state :corp ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (play-from-hand state :corp "Crisium Grid" "Archives")
    (play-from-hand state :corp "Crisium Grid" "R&D")
    (trash-from-hand state :corp "Project Atlas")
    (trash-from-hand state :corp "Shock!")
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (prompt-choice :runner "Card from hand")
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :hq 0))
    (is (not (get-content state :hq 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "R&D")
    (prompt-choice :runner "Card from deck")
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :rd 0))
    (is (not (get-content state :rd 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "Archives")
    (prompt-choice :runner "Shock!")
    (prompt-choice :runner "Project Atlas")
    (prompt-choice :runner "Steal")
    (prompt-select :runner (get-content state :archives 0))
    (is (not (get-content state :archives 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")))

(deftest maxx
  (do-game
    (new-game (default-corp)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                    "Eater"]))
    (starting-hand state :runner ["Eater"])
    (take-credits state :corp)
    (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
    (is (last-log-contains? state "Wyldside, Wyldside")
        "Maxx did log trashed card names")))

(deftest maxx-dummy-box
  ; Check that mills don't trigger trash prevention #3246
  (do-game
    (new-game (default-corp)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Dummy Box" 30)]))
    (take-credits state :corp)
    (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
    (play-from-hand state :runner "Dummy Box")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (empty? (:prompt (get-runner))) "Dummy Box not fired from mill")))

(deftest maxx-wyldside-start-of-turn
  ;; MaxX and Wyldside - using Wyldside during Step 1.2 should lose 1 click
  (do-game
    (new-game (default-corp)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                     (qty "Sure Gamble" 3)
                                                     (qty "Infiltration" 3)
                                                     (qty "Corroder" 3)
                                                     (qty "Eater" 3)]))
    (take-credits state :corp)
    (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
    (starting-hand state :runner ["Wyldside"])
    (play-from-hand state :runner "Wyldside")
    (take-credits state :runner 3)
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits at end of first turn")
    (is (find-card "Wyldside" (get-in @state [:runner :rig :resource])) "Wyldside was installed")
    (take-credits state :corp)
    (is (= 0 (:click (get-runner))) "Runner has 0 clicks")
    (is (:runner-phase-12 @state) "Runner is in Step 1.2")
    (let [maxx (get-in @state [:runner :identity])
          wyld (find-card "Wyldside" (get-in @state [:runner :rig :resource]))]
      (card-ability state :runner maxx 0)
      (card-ability state :runner wyld 0)
      (core/end-phase-12 state :runner nil)
      (is (= 4 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (= 3 (:click (get-runner))) "Wyldside caused 1 click to be lost")
      (is (= 3 (count (:hand (get-runner)))) "3 cards drawn total"))))

(deftest nasir-ability-basic
  ;; Nasir Ability - Basic
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Nasir Meidan: Cyber Explorer" []))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)

    (run-on state "HQ")
    (let [iwall (get-ice state :hq 0)
          nasir (get-in @state [:runner :identity])]
      (core/rez state :corp iwall)
      (is (= 5 (:credit (get-runner))) "Nasir Ability does not trigger automatically")
      (card-ability state :runner nasir 0)
      (is (= 1 (:credit (get-runner))) "Credits at 1 after Nasir ability trigger"))))

(deftest nasir-ability-xanadu
  ;; Nasir Ability - Xanadu
  (do-game
    (new-game
      (default-corp ["Ice Wall"])
      (make-deck "Nasir Meidan: Cyber Explorer" ["Xanadu"]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)

    (swap! state assoc-in [:runner :credit] 6)
    (play-from-hand state :runner "Xanadu")
    (run-on state "HQ")
    (let [iwall (get-in @state [:corp :servers :hq :ices 0])
          nasir (get-in @state [:runner :identity])]
      (core/rez state :corp iwall)
      (is (= 3 (:credit (get-runner))) "Pay 3 to install Xanadu")
      (card-ability state :runner nasir 0)
      (is (= 2 (:credit (get-runner))) "Gain 1 more credit due to Xanadu"))))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message
  (testing "Trace to tag Runner when first installed Corp card is trashed"
    (do-game
      (new-game
        (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 3)])
        (default-runner ["Forger"]))
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Forger")
      ; trash from HQ first - #2321
      (run-empty-server state "HQ")
      (prompt-choice-partial :runner "Pay")
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (prompt-choice :corp "Yes")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (empty? (:prompt (get-runner))) "Forger can't avoid the tag")
      (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 2")
      (prompt-choice-partial :runner "Pay")
      (is (empty? (:prompt (get-corp))) "No trace chance on 2nd trashed card of turn")))
  (testing "Interaction with Dedicated Response Team"
    (do-game
      (new-game
        (make-deck "NBN: Controlling the Message" ["Launch Campaign" "Dedicated Response Team"])
        (default-runner))
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "Pay")
      (prompt-choice :corp "Yes")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from DRT"))))

(deftest new-angeles-sol-on-steal
  ;; New Angeles Sol - interaction with runner stealing agendas
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Paywall Implementation" 2) "Breaking News"])
      (default-runner))
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Paywall Implementation")
    (take-credits state :corp)
    (is (= 6 (:credit (get-corp))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-corp))) "Corp gained 1cr from successful run")
    (prompt-choice :runner "Steal")
    (prompt-choice :corp "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-corp))) "Paywall trashed before Sol triggers")
    (prompt-select :corp (find-card "Paywall Implementation" (:hand (get-corp))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall back in play")))

(deftest next-design
  ;; Next Design.  Install up to 3 ICE before game starts, one per server max, and re-draw to 5
  (do-game
    (new-game
      (make-deck "NEXT Design: Guarding the Net" [(qty "Snowflake" 10)])
      (default-runner)
      {:dont-start-turn true})
    (prompt-select :corp (find-card "Snowflake" (:hand (get-corp))))
    (prompt-choice :corp "HQ")
    (prompt-select :corp (find-card "Snowflake" (:hand (get-corp))))
    (prompt-choice :corp "R&D")
    (prompt-select :corp (find-card "Snowflake" (:hand (get-corp))))
    (prompt-choice :corp "New remote")
    (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (= 5 (count (:hand (get-corp)))) "Corp should start with 5 cards in hand")))

(deftest nisei-division
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game
      (make-deck "Nisei Division: The Next Generation" [(qty "Snowflake" 2)])
      (default-runner))
    (play-from-hand state :corp "Snowflake" "HQ")
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (let [s1 (get-in @state [:corp :servers :hq :ices 0])
          s2 (get-in @state [:corp :servers :hq :ices 1])]
      (run-on state "HQ")
      (core/rez state :corp s2)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s2 0)
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game")
      (core/no-action state :corp nil)
      (core/rez state :corp s1)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s1 0)
      (prompt-choice :corp "0 [Credits]")
      (prompt-choice :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game"))))

(deftest noise-ability
  ;; Noise: Hacker Extraordinaire - Ability
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)])
      (make-deck "Noise: Hacker Extraordinaire" ["Datasucker" "Cache" "Sure Gamble" (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]))
    (starting-hand state :runner ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-corp)))) "Corp should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-corp)))) "Corp deck should contain 5 cards")
    (take-credits state :corp)
    (is (= 0 (count (:discard (get-corp)))) "Archives started empty")
    (play-from-hand state :runner "Datasucker")
    (is (= 1 (count (:discard (get-corp)))) "Playing virus should cause card to be trashed from R&D")
    (is (= 4 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 1 (count (:discard (get-corp)))) "Playing non-virus should not cause card to be trashed from R&D")
    (core/click-draw state :runner nil)
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Clone Chip")
    (trash-from-hand state :runner "Cache")
    (trash-from-hand state :runner "Sharpshooter")
    (take-credits state :runner)
    ;; playing virus via Clone Chip on Corp's turn should trigger Noise ability
    (let [chip (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner chip 0)
      (prompt-select :runner (find-card "Cache" (:discard (get-runner))))
      (let [ds (get-in @state [:runner :rig :program 1])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing virus via Clone Chip on corp's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
    (let [chip-2 (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner chip-2 0)
      (prompt-select :runner (find-card "Sharpshooter" (:discard (get-runner))))
      (let [ss (get-in @state [:runner :rig :program 2])]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing non-virus via Clone Chip on corp's turn should not trigger Noise ability")))

(deftest null-ability
  ;; Null ability - once per turn
  (do-game
    (new-game
      (default-corp [(qty "Wraparound" 3)])
      (make-deck "Null: Whistleblower" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Wraparound" "HQ")
    (play-from-hand state :corp "Wraparound" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [null (get-in @state [:runner :identity])
          wrap1 (get-ice state :hq 0)
          wrap2 (get-ice state :hq 1)]
      (card-ability state :runner null 0)
      (is (empty? (:prompt (get-runner))) "Ability won't work on unrezzed ICE")
      (core/rez state :corp wrap2)
      (card-ability state :runner null 0)
      (prompt-select :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
      (run-continue state)
      (core/rez state :corp wrap1)
      (card-ability state :runner null 0)
      (is (empty? (:prompt (get-runner))) "Ability already used this turn")
      (run-jack-out state)
      (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))

(deftest null-trashed
  ;; Null ability - does not affect next ice when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-corp ["Wraparound" "Spiderweb"])
      (make-deck "Null: Whistleblower" [(qty "Parasite" 3)]))
    (play-from-hand state :corp "Spiderweb" "HQ")
    (play-from-hand state :corp "Wraparound" "HQ")
    (take-credits state :corp)
    (core/gain state :corp :credit 10)
    (let [null (get-in @state [:runner :identity])
          spider (get-ice state :hq 0)
          wrap (get-ice state :hq 1)]
      (core/rez state :corp spider)
      (core/rez state :corp wrap)
      (play-from-hand state :runner "Parasite")
      (prompt-select :runner (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner null 0)
      (prompt-select :runner (first (:hand (get-runner))))
      (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Null")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null"))))

(deftest omar
  ;; Omar Keung
  (testing "Make a successful run on the chosen server once per turn"
    (do-game
      (new-game
        (default-corp)
        (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])]
        (card-ability state :runner omar 0)
        (run-successful state)
        (prompt-choice :runner "HQ")
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (prompt-choice :runner "No action")
        (is (= 3 (:click (get-runner))))
        (card-ability state :runner omar 0)
        (is (= 3 (:click (get-runner))))
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :rd)
        (is (= [:rd] (get-in @state [:runner :register :successful-run])))
        (card-ability state :runner omar 0)
        (run-successful state)
        (prompt-choice :runner "HQ")
        (is (= [:hq :rd] (get-in @state [:runner :register :successful-run]))))))
  (testing "Ash prevents access, but not successful run"
    (do-game
      (new-game
        (default-corp ["Ash 2X3ZB9CY"])
        (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            ash (get-content state :hq 0)]
        (core/rez state :corp ash)
        (card-ability state :runner omar 0)
        (run-successful state)
        (prompt-choice :runner "HQ")
        (prompt-choice :corp 0)
        (prompt-choice :runner 0)
        (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)))
        (is (= :hq (-> (get-runner) :register :successful-run first))))))
  (testing "Crisium Grid prevents prompt"
    (do-game
      (new-game
        (default-corp ["Crisium Grid"])
        (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
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
      (new-game
        (default-corp)
        (make-deck "Omar Keung: Conspiracy Theorist" ["Medium"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Medium")
      (let [omar (get-in @state [:runner :identity])
            medium (get-in @state [:runner :rig :program 0])]
        (card-ability state :runner omar 0)
        (run-successful state)
        (prompt-choice :runner "R&D")
        (is (= 1 (get-counters (refresh medium) :virus))))))
  (testing "When selecting HQ, ability adds counters to Nerve Agent"
    (do-game
      (new-game
        (default-corp)
        (make-deck "Omar Keung: Conspiracy Theorist" ["Nerve Agent"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Nerve Agent")
      (let [omar (get-in @state [:runner :identity])
            nerve (get-in @state [:runner :rig :program 0])]
        (card-ability state :runner omar 0)
        (run-successful state)
        (prompt-choice :runner "HQ")
        (is (= 1 (get-counters (refresh nerve) :virus)))))))

(deftest quetzal
  ;; Quetzal
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Quetzal: Free Spirit" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [q (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)
          qdef (core/card-def (get-in @state [:runner :identity]))
          qmsg (get-in qdef [:abilities 0 :msg])]
      (core/rez state :corp iwall)
      (card-ability state :runner q 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (not (last-log-contains? state qmsg)) "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (core/jack-out state :runner nil))))

(deftest reina-rez
  ;; Reina Roja - Increase cost of first rezzed ICE
  (do-game
    (new-game
      (default-corp [(qty "Quandary" 3)])
      (make-deck "Reina Roja: Freedom Fighter" []))
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (core/rez state :corp quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))

(deftest rielle-kit-peddler
  ;; Rielle "Kit" Peddler - Give ICE Code Gate
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 2)])
              (make-deck "Rielle \"Kit\" Peddler: Transhuman" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [k (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)]
      (core/rez state :corp iwall)
      (card-ability state :runner k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))

(deftest skorpios
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game (make-deck "Skorpios Defense Systems: Persuasive Power" ["Hedge Fund" (qty "Quandary" 4)])
              (default-runner ["The Maker's Eye" "Lucky Find"]))
    (play-from-hand state :corp "Hedge Fund")
    (dotimes [_ 4] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (play-from-hand state :runner "Lucky Find")
    (play-from-hand state :runner "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-corp) :prompt first :choices))) "No Maker's Eye choice")
    (prompt-choice :corp "Cancel")
    (run-successful state)
    (prompt-choice :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "1st quandary")
    (prompt-choice :runner "No action")
    (prompt-choice :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "2nd quandary")
    (prompt-choice :runner "No action")
    (prompt-choice :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "3rd quandary")
    (prompt-choice :runner "No action")
    (is (not (:run @state)))
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (prompt-card :corp (find-card "The Maker's Eye" (:discard (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rfg]))) "One card RFGed")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (:prompt (get-corp))) "Cannot use Skorpios twice")))

(deftest silhouette
  ;; Silhouette
  (testing "Expose trigger ability resolves completely before access. Issue #2173"
    (do-game
      (new-game
        (default-corp ["Psychic Field" (qty "Fetal AI" 10)])
        (make-deck "Silhouette: Stealth Operative" ["Feedback Filter" "Inside Job"]))
      (starting-hand state :corp ["Psychic Field" "Fetal AI"])
      (play-from-hand state :corp "Psychic Field" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let [psychic (get-content state :remote1 0)
            ff (get-hardware state 0)]
        (run-empty-server state :hq)
        (is (:run @state) "On successful run trigger effects")
        (prompt-select :runner psychic)
        (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
        (prompt-choice :corp "2 [Credits]")
        (prompt-choice :runner "0 [Credits]")
        (card-ability state :runner ff 0)
        (prompt-choice :runner "Done")
        (is (= 0 (:credit (get-runner))) "Runner has no more credits left")
        (is (= 1 (count (:hand (get-runner)))) "Prevented 1 net damage")
        (is (empty? (:discard (get-runner))) "No cards discarded")
        (is (:run @state) "On run access phase")
        (prompt-choice :runner "Done")
        (is (empty? (:hand (get-runner))) "Suffered 1 net damage due to accessing Fetal AI")
        (is (= 1 (count (:discard (get-runner)))) "Discarded 1 card due to net damage")
        (is (:run @state) "Resolving access triggers")
        (prompt-choice :runner "No action")
        (is (= 0 (count (:scored (get-runner)))) "Runner has no credits to be able to steal Fetal AI")
        (is (not (:run @state)) "Run has now ended")
        (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
  (testing "with Temüjin; broken interaction with other successful-run triggers. Issue #1968"
    (do-game
      (new-game
        (default-corp ["PAD Campaign" (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)])
        (make-deck "Silhouette: Stealth Operative" ["Temüjin Contract" "Desperado"]))
      (starting-hand state :corp ["Hedge Fund" "PAD Campaign"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Temüjin Contract")
      (prompt-choice :runner "HQ")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (prompt-choice :runner "Temüjin Contract")
      (prompt-select :runner (get-content state :remote1 0))
      (prompt-choice :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
      (is (= 8 (:credit (get-runner))) "Gained 4cr")
      ;; second run
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 12 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin"))))

(deftest spark-advertisements
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game
      (make-deck "Spark Agency: Worldswide Reach" [(qty "Launch Campaign" 3)])
      (default-runner))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [lc1 (get-content state :remote1 0)
          lc2 (get-content state :remote2 0)
          lc3 (get-content state :remote3 0)]
      (core/rez state :corp lc1)
      (is (= 4 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (core/rez state :corp lc3)
      (is (= 4 (:credit (get-runner)))
          "Runner did not lose credit from second Spark rez")
      (take-credits state :corp)
      (run-on state "Server 1")
      (core/rez state :corp lc2)
      (is (= 3 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Runner turn)"))))

(deftest sso-industries-fueling-innovation
  ;; SSO Industries: Fueling Innovation - add advancement tokens on ice for faceup agendas
  (do-game
    (new-game
      (make-deck "SSO Industries: Fueling Innovation"
                 [(qty "Hortum" 2) (qty "Oaktown Renovation" 2) "Braintrust"])
      (default-runner))
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (is (empty? (:prompt (get-corp))) "Not prompted when no faceup agenda available")
    (take-credits state :runner)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (take-credits state :corp)
    (is (empty? (:prompt (get-corp))) "Not prompted when no ice available")
    (take-credits state :runner)
    (play-from-hand state :corp "Hortum" "HQ")
    (play-from-hand state :corp "Hortum" "R&D")
    (let [h0 (get-ice state :hq 0)
          h1 (get-ice state :rd 0)]
      (is (= nil (:advance-counter (refresh h0))) "Starts with 0 tokens")
      (is (= nil (:advance-counter (refresh h1))) "Starts with 0 tokens")
      (take-credits state :corp)
      (prompt-choice :corp "Yes")
      (prompt-select :corp (refresh h0))
      (is (= 2 (:advance-counter (refresh h0))) "Gains 2 tokens")
      (is (= nil (:advance-counter (refresh h1))) "Stays at 0 tokens")
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (take-credits state :corp)
      (prompt-choice :corp "Yes")
      (prompt-select :corp (refresh h1))
      (is (= 2 (:advance-counter (refresh h0))) "Stays at 2 tokens")
      (is (= 4 (:advance-counter (refresh h1))) "Gains 4 tokens")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (empty? (:prompt (get-corp))) "Not prompted when all ice advanced"))))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward - Ability
  (do-game
    (new-game
      (make-deck "Strategic Innovations: Future Forward"
                 [(qty "Hedge Fund" 2) (qty "Eli 1.0" 2) (qty "Crick" 2)])
      (default-runner))
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Crick" "New remote")
    (let [i1 (get-ice state :remote1 0)
          i2 (get-ice state :remote2 0)]
      (take-credits state :corp 0)
      (take-credits state :runner)
      (core/rez state :corp i1)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (count (:prompt (get-corp)))) "Corp prompted to trigger Strategic Innovations")
      (prompt-select :corp (first (:discard (get-corp))))
      (is (empty? (:discard (get-corp))) "Hedge Fund moved back to R&D")
      (take-credits state :corp)
      (core/rez state :corp i2)
      (take-credits state :runner)
      (is (= 0 (count (:prompt (get-corp))))
          "Corp not prompted to trigger Strategic Innovations"))))

(deftest the-foundry
  ;; The Foundry
  (testing "interaction with Accelerated Beta Test"
    (do-game
      (new-game
        (make-deck "The Foundry: Refining the Process" [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)])
        (default-runner))
      (starting-hand state :corp ["Accelerated Beta Test"])
      (play-from-hand state :corp "Accelerated Beta Test" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (prompt-choice :corp "Yes")
      (prompt-select :corp (find-card "Eli 1.0" (:play-area (get-corp))))
      (prompt-choice :corp "Archives")
      (prompt-choice :corp "Yes")
      (is (empty? (:play-area (get-corp))) "Play area shuffled into R&D"))))

(deftest the-outfit
  ;; The Outfit - Gain 3 whenever you take at least 1 bad publicity
  (testing "basic test"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Hostile Takeover" "Profiteering"])
        (default-runner))
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 1 (:bad-publicity (get-corp))) "Take 1 bad publicity")
      (is (= 15 (:credit (get-corp))) "Corp should gain 10 credits")
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (prompt-choice :corp "3")  ;; Take 3 bad publicity from Profiteering, gain 15
      (is (= 4 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity")
      (is (= 33 (:credit (get-corp))) "Corp should gain 18 credits")))
  (testing "with Profiteering - Only gain 3 credits when taking more than 1 bad publicity in a single effect"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Profiteering"])
        (default-runner))
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (prompt-choice :corp "3")
      (is (= 3 (:bad-publicity (get-corp))) "Take 3 bad publicity")
      (is (= 23 (:credit (get-corp))) "Gain 15 from Profiteering + 3 from The Outfit")))
  (testing "vs Valencia - 1 bad pub at start means 8 credits to start with"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Hostile Takeover"])
        (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
      (is (= 1 (:bad-publicity (get-corp))) "The Outfit starts with 1 bad publicity")
      (is (= 8 (:credit (get-corp))) "The Outfit starts with 8 credits")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 2 (:bad-publicity (get-corp))) "Take 1 bad publicity")
      (is (= 18 (:credit (get-corp))) "Gain 7 from Hostile Takeover + 3 from The Outfit"))))

(deftest titan-transnational
  ;; Titan Transnational
  (testing "Add a counter to a scored agenda"
    (do-game
      (new-game
        (make-deck "Titan Transnational: Investing In Your Future" ["Project Atlas"])
        (default-runner))
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atl (get-content state :remote1 0)]
        (core/gain state :corp :click 1)
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/score state :corp {:card (refresh atl)})
        (let [scored (get-in @state [:corp :scored 0])]
          (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))
  (testing "only use one counter of Corporate Sales Team"
    (do-game
      (new-game
        (make-deck "Titan Transnational: Investing In Your Future" ["Corporate Sales Team" "Mark Yale"])
        (default-runner))
      (play-from-hand state :corp "Corporate Sales Team" "New remote")
      (play-from-hand state :corp "Mark Yale" "New remote")
      (let [cst (get-content state :remote1 0)
            my (get-content state :remote2 0)]
        (core/gain state :corp :click 3)
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/score state :corp {:card (refresh cst)})
        (let [scored (get-in @state [:corp :scored 0])]
          (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
          (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
          (core/rez state :corp my)
          (card-ability state :corp my 1)
          (prompt-select :corp (refresh scored))
          (is (= 0 (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
          (card-ability state :corp my 1)
          (prompt-select :corp (refresh scored))
          (is (= 0 (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale"))))))

(deftest weyland-builder-of-nations
  ;; Builder of Nations
  (testing "1 meat damage per turn at most"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" [(qty "Hedge Fund" 3)])
        (default-runner))
      (let [bon (get-in @state [:corp :identity])]
        (card-ability state :corp bon 0)
        (prompt-choice :corp "Cancel")
        (is (= 0 (count (:discard (get-runner)))) "Runner took no meat damage from BoN")
        (card-ability state :corp bon 0)
        (prompt-choice :corp "Yes")
        (is (= 1 (count (:discard (get-runner)))) "Runner took 1 meat damage from BoN")
        (card-ability state :corp bon 0)
        (is (= 1 (count (:discard (get-runner)))) "Runner took only 1 meat damage from BoN total")
        (is (= 0 (count (:prompt (get-corp))))))))
  (testing "2 meat damage from ID ability when The Cleaners is scored"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" [(qty "The Cleaners" 3) (qty "Ice Wall" 3)])
        (default-runner [(qty "Sure Gamble" 2)]))
      (play-from-hand state :corp "The Cleaners" "New remote")
      (let [clean (get-content state :remote1 0)]
        (score-agenda state :corp clean)
        (let [bon (get-in @state [:corp :identity])]
          (card-ability state :corp bon 0)
          (prompt-choice :corp "Yes")
          (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from BoN/Cleaners combo"))))))

(deftest whizzard
  ;; Whizzard - Recurring credits
  (do-game
    (new-game (default-corp) (make-deck "Whizzard: Master Gamer" ["Sure Gamble"]))

    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :runner (:identity (get-runner)) 0)))]
      (is (changes-credits (get-runner) 1 (click-whizzard 1)))
      (is (changes-credits (get-runner) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")

      (take-credits state :corp)
      (is (changes-credits (get-runner) 3 (click-whizzard 3)) "Credits reset at start of Runner's turn")

      (take-credits state :runner)
      (is (changes-credits (get-runner) 0 (click-whizzard 1)) "Credits don't reset at start of Corp's turn"))))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced - Ability
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 3)])
              (make-deck "Wyvern: Chemically Enhanced"
                         [(qty "Sure Gamble" 2) "Corroder"
                          "Clone Chip" "Easy Mark"]))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Easy Mark")
    (play-from-hand state :runner "Corroder")
    (run-empty-server state "Server 1")
    (prompt-choice-partial :runner "Pay")  ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-runner))) "Easy Mark moved to deck")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (run-empty-server state "Server 2")
    (prompt-choice-partial :runner "Pay")
    (is (= "Sure Gamble" (:title (last (:discard (get-runner))))) "Sure Gamble still in Wyvern's discard")))
