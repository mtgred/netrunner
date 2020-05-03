(ns game.cards.identities-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest ^{:card-title "419-amoral-scammer"}
  FourHundredAndNineTeen-amoral-scammer
  ;; 419
  (testing "basic test: Amoral Scammer - expose first installed card unless corp pays 1 credit"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck ["PAD Campaign" "The Cleaners" (qty "Pup" 3) "Oaktown Renovation"]}
                 :runner {:id "419: Amoral Scammer"}})
      (is (= 5 (:credit (get-corp))) "Starts with 5 credits")
      (play-from-hand state :corp "Pup" "HQ")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "Yes")
      (is (= 4 (:credit (get-corp))) "Pays 1 credit to not expose card")
      (play-from-hand state :corp "Pup" "HQ")
      (is (empty? (:prompt (get-runner))) "No option on second install")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Archives")
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-corp))) "No prompt if Runner chooses No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "The Cleaners" "New remote")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (is (last-log-contains? state "exposes The Cleaners") "Installed card was exposed")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (empty? (:prompt (get-corp))) "Cannot expose faceup agendas")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/lose state :corp :credit (:credit (get-corp)))
      (is (zero? (:credit (get-corp))) "Corp has no credits")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (click-prompt state :runner "Yes")
      (is (empty? (:prompt (get-corp))) "No prompt if Corp has no credits")
      (is (last-log-contains? state "exposes PAD Campaign") "Installed card was exposed")))
  (testing "Verify expose can be blocked"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck ["Underway Grid" "Pup"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Underway Grid" "New remote")
      (click-prompt state :runner "No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Server 1")
      (click-prompt state :runner "Yes")
      (let [ug (get-in @state [:corp :servers :remote1 :content 0])]
        (core/rez state :corp ug)
        (click-prompt state :corp "No")
        (is (last-log-contains? state "uses Underway Grid to prevent 1 card from being exposed") "Exposure was prevented"))))
  (testing "Ixodidae shouldn't trigger off 419's ability"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:id "419: Amoral Scammer"
                          :deck ["Ixodidae"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Ixodidae")
      (take-credits state :runner)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (let [corp-credits (:credit (get-corp))
            runner-credits (:credit (get-runner))]
        (click-prompt state :runner "Yes")
        (click-prompt state :corp "Yes")
        (is (= 1 (- corp-credits (:credit (get-corp)))) "Should lose 1 credit from 419 ability")
        (is (zero? (- runner-credits (:credit (get-runner)))) "Should not gain any credits from Ixodidae"))))
  (testing "419 vs Asa Group double install, Corp's turn"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["PAD Campaign" "Ice Wall"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (click-card state :corp "Ice Wall")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (let [log (-> @state :log last :text)]
        (is (= log "Runner exposes PAD Campaign in Server 1.")))))
  (testing "419 vs Asa Group double install, Runner's turn"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["PAD Campaign" "Ice Wall" "Advanced Assembly Lines"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Advanced Assembly Lines" "New remote")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "No")
      (take-credits state :corp)
      (core/rez state :corp (get-content state :remote1 0))
      (card-ability state :corp (get-content state :remote1 0) 0)
      (click-card state :corp "PAD Campaign")
      (click-prompt state :corp "New remote")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (let [log (-> @state :log last :text)]
        (is (= log "Runner exposes PAD Campaign in Server 2.")))
      (is (prompt-is-type? state :corp :select) "Corp should still have select prompt")))
  (testing "interation with 'install and rez' effects. Issue #4485"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Building Blocks" "Ice Wall"]
                        :credits 10}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Building Blocks")
      (click-card state :corp "Ice Wall")
      (click-prompt state :corp "HQ")
      (is (empty? (:prompt (get-runner))) "419 doesn't trigger on installed and rezzed cards")))
  (testing "419 vs Sportsmetal Jinja Grid. Issue #3806"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck [(qty "Ice Wall" 5)]
                        :hand ["Domestic Sleepers" "Jinja City Grid"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Jinja City Grid" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (click-prompt state :runner "No")
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Draw 2 cards")
      (is (prompt-is-type? state :runner :waiting) "During Jinja, Runner should wait")
      (click-prompt state :corp (first (prompt-buttons :corp)))
      (is (= 2 (count (prompt-buttons :runner))) "419 can trigger ability with 2 options")
      (is (prompt-is-type? state :corp :waiting) "Corp is waiting for runner ability")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (is (= 2 (count (prompt-buttons :corp))) "Corp should have prompt back with 2 options")
      (is (prompt-is-type? state :runner :waiting) "Runner should wait again"))))

(deftest acme-consulting-the-truth-you-need
  (testing "Tag gain when rezzing outermost ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (is-tagged? state)) "Runner does not encounter an unrezzed ice")
      (core/rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner no longer encountering outermost ice")))
  (testing "Interaction with Data Ward"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Data Ward"]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Data Ward" "Archives")
      (take-credits state :corp)
      (run-on state "Archives")
      (is (not (is-tagged? state)) "Runner does not encounter an unrezzed ice")
      (core/rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Take 1 tag")
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (card-subroutine state :corp (get-ice state :archives 0) 0)
      (is (not (:run @state)) "Run ended by Data Ward")))
  (testing "Tag gain when starting run"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner no longer encountering outermost ice")))
  (testing "Tag loss when derezzing ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (core/derez state :corp (get-ice state :archives 0))
      (is (not (is-tagged? state)) "Runner no longer encountering the derezzed ice")))
  (testing "No tag on empty server"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (not (is-tagged? state)) "No ice to encounter")))
  (testing "No tag when encountering second ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Vanilla" 2) (qty "Hedge Fund" 4)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (core/rez state :corp (get-ice state :archives 1))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner is not tagged when encountering second ice")))
  (testing "Tag loss when runner jacks out"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-jack-out state)
      (is (not (is-tagged? state)) "Runner no longer tagged after jacking out")))
  (testing "No tag gained when rezzing something other than ice"
    (do-game
      (new-game
        {:corp {:id "Acme Consulting: The Truth You Need"
                :deck ["Vanilla" "NGO Front"]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "NGO Front" "New remote")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (is-tagged? state)) "Runner is not yet tagged when encountering outermost ice")
      (core/rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (= 1 (get-in @state [:runner :tag :additional])) "Runner gains 1 additional tag when ice rezzed")
      (core/rez state :corp (get-content state :remote1 0))
      (is (rezzed? (get-content state :remote1 0)) "NGO Front now rezzed")
      (is (= 1 (get-in @state [:runner :tag :additional])) "Runner does not gain a tag when asset rezzed")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner is not tagged when encountering second ice")))
  (testing "Trashing the ice removes the tag #4984"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Ice Wall"]}
                 :runner {:deck ["Corroder" "Hippo"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Corroder")
      (run-on state "Server 1")
      (core/rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (is (zero? (count-tags state)) "Acme additional tag falls off"))))

(deftest adam-compulsive-hacker
  ;; Adam
  (testing "Allow runner to choose directives"
    (do-game
      (new-game {:runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}
                 :options {:dont-start-game true}})
      (is (= 4 (count (get-in @state [:runner :play-area]))) "All directives are in the runner's play area")
      (is (zero? (count (get-in @state [:runner :hand]))))
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (is (= 3 (count (get-resource state))) "3 directives were installed")
      (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
      (let [nat (find-card "Neutralize All Threats" (get-resource state))
            sf (find-card "Safety First" (get-resource state))
            abr (find-card "Always Be Running" (get-resource state))]
        (is (and nat sf abr) "The chosen directives were installed"))))
  (testing "Directives should not grant Pālanā credits"
    (do-game
      (new-game {:corp {:id "Pālanā Foods: Sustainable Growth"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}
                 :options {:dont-start-game true}})
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (click-prompt state :corp "Keep")
      (click-prompt state :runner "Keep")
      (core/start-turn state :corp nil)
      (is (= 5 (:credit (get-corp))) "Pālanā does not gain credit from Adam's starting Directives")))
  (testing "Neutralize All Threats interaction with advanceable traps"
    (do-game
      (new-game {:corp {:deck [(qty "Cerebral Overwriter" 3)]}
                 :runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}
                 :options {:dont-start-game true}})
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (click-prompt state :corp "Keep")
      (click-prompt state :runner "Keep")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (advance state (get-content state :remote1 0) 2)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "1 card in archives"))))

(deftest aginfusion-new-miracles-for-a-new-world
  (testing "Ability works. #5056"
    (do-game
      (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund" "Ice Wall" "Kakugo"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Kakugo" "HQ")
      (take-credits state :corp)
      (run-on state "R&D")
      (is (= (get-ice state :rd 0) (core/get-current-ice state)))
      (card-ability state :corp (:identity (get-corp)) 0)
      (click-prompt state :corp "HQ")
      (let [ice-passed-last-run
            (->> (:events (get-run))
                 (filter #(= :pass-ice (first %)))
                 (keep second))]
        (is (= 1 (count ice-passed-last-run)))
        (is (utils/same-card? (get-ice state :hq 0) (ffirst ice-passed-last-run))))
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))))))
  (testing "Works with passing effects"
    (do-game
      (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund" "Ice Wall" "Kakugo"]}
                 :runner {:hand ["En Passant"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Kakugo" "HQ")
      (take-credits state :corp)
      (run-on state "R&D")
      (is (= (get-ice state :rd 0) (core/get-current-ice state)))
      (card-ability state :corp (:identity (get-corp)) 0)
      (click-prompt state :corp "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "En Passant")
      (click-card state :runner "Kakugo")
      (is (nil? (get-ice  state :hq 0)))
      (is (find-card "Kakugo" (:discard (get-corp))))))
  (testing "Uses up on-encounter bypass effects"
    (do-game
      (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund" "Ice Wall" "Vanilla" "Kakugo"]
                        :credits 10}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Kakugo" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "R&D")
      (is (= (get-ice state :rd 0) (core/get-current-ice state)))
      (card-ability state :corp (:identity (get-corp)) 0)
      (click-prompt state :corp "HQ")
      (is (= (get-ice state :hq 0) (core/get-current-ice state)))
      (run-continue state)
      (is (last-log-contains? state "Runner encounters Kakugo protecting HQ at position 0.")))))

(deftest akiko-nisei-head-case
  ;; Akiko Nisei
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand [(qty "Hedge Fund" 4)]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 2 (:total (core/num-cards-to-access state :runner :rd nil))) "Should access additional card from ability")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 1 (:total (core/num-cards-to-access state :runner :rd nil))) "Should only access 1 from missed psi game")))
  (testing "Shiro interaction: second sub should give Akiko 2 accesses"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Shiro"]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Shiro" "New remote")
      (take-credits state :corp)
      (let [shiro (get-ice state :remote1 0)]
        (core/rez state :corp shiro)
        (run-on state :remote1)
        (run-continue state)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "No")
        (run-continue state)
        (run-continue state)
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 2 (:total (core/num-cards-to-access state :runner :rd nil))) "Should access additional card from ability")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (run-successful state)
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :remote1)
        (run-continue state)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "No")
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 1 (:total (core/num-cards-to-access state :runner :rd nil))) "Should only access 1 from missed psi game")))))

(deftest alice-merchant-clan-agitator
  ;; Alice Merchant
  (do-game
    (new-game {:runner {:id "Alice Merchant: Clan Agitator"
                        :deck ["Security Testing"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Security Testing")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "Archives")
    (run-empty-server state "Archives")
    (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (is (= 1 (-> (get-corp) :discard count)) "Alice ability should trash 1 card from HQ")
    (is (-> (get-corp) :discard first :seen not) "Discarded card should be facedown when access is replaced")))

(deftest andromeda-dispossessed-ristie
  ;; Andromeda - 9 card starting hand, 1 link
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 1 (:link (get-runner))) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))
  (testing "9 card starting hand after mulligan"
    (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}
                 :options {:mulligan :runner}})
      (is (= 1 (:link (get-runner))) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))
  (testing "should not grant Palana credits"
    (do-game
      (new-game {:corp {:id "Pālanā Foods: Sustainable Growth"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 5 (:credit (get-corp))) "Palana does not gain credit from Andromeda's starting hand"))))

(deftest apex-invasive-predator
  ;; Apex
  (testing "Allow facedown install of a second console. Issue #1326"
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck [(qty "Heartbeat" 2)]}})
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (click-prompt state :runner "Done")
      (play-from-hand state :runner "Heartbeat")
      (is (= 1 (count (get-hardware state))))
      (take-credits state :runner)
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
      (is (= 1 (count (get-runner-facedown state))) "2nd console installed facedown")))
  (testing "Don't fire events when installed facedown. Issue #4085"
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Sure Gamble"]}})
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (let [credits (:credit (get-runner))]
        (click-card state :runner "Sure Gamble")
        (is (= credits (:credit (get-runner)))))))
  (testing "Spec Work cannot be installed facedown #4574"
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Spec Work" "Sure Gamble" "Cache"]}})
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (click-card state :runner "Spec Work")
      (is (= 1 (count (get-runner-facedown state))) "Spec Work installed facedown"))))

(deftest asa-group-security-through-vigilance
  (testing "Asa Group should not allow installing operations"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "BOOM!" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "BOOM!" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))
  (testing "Asa Group should not allow installing agendas"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "Project Vitruvius" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "Project Vitruvius" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group did not install Agenda with its ability")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))
  (testing "Asa Group ordering correct when playing Mirrormorph"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Shipment from MirrorMorph"
                               "Pup"
                               "Red Herrings"
                               "Marilyn Campaign"
                               "Project Vitruvius"]}})
      (let  [marilyn (find-card "Marilyn Campaign" (:hand (get-corp)))
             pup (find-card "Pup" (:hand (get-corp)))
             herrings (find-card "Red Herrings" (:hand (get-corp)))
             vitruvius (find-card "Project Vitruvius" (:hand (get-corp)))]
        (play-from-hand state :corp "Shipment from MirrorMorph")
        (click-card state :corp marilyn)
        (click-prompt state :corp "New remote")
        (is (= "Marilyn Campaign" (:title (get-content state :remote1 0))) "Marilyn is installed as first card")
        (is (= "Select a non-agenda in HQ to install" (:msg (prompt-map :corp))))
        (click-card state :corp herrings)
        (is (= "Red Herrings" (:title (get-content state :remote1 1))) "Red Herrings is installed in Server 1")
        (click-card state :corp vitruvius)
        (click-prompt state :corp "New remote")
        (click-card state :corp pup)
        (click-prompt state :corp "New remote")
        (is (empty? (:prompt (get-corp))) "No more prompts")
        (is (= 6 (count (:servers (get-corp)))) "There are six servers, including centrals")))))

(deftest ayla-bios-rahim-simulant-specialist
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game {:runner {:id "Ayla \"Bios\" Rahim: Simulant Specialist"
                        :deck ["Sure Gamble" "Desperado"
                               "Security Testing" "Bank Job"
                               "Heartbeat" "Eater"]}
               :options {:dont-start-game true}})
    (is (= 6 (count (get-in @state [:runner :play-area]))) "Deck cards are in play area")
    (is (zero? (count (get-in @state [:runner :hand]))))
    (click-card state :runner (find-card "Sure Gamble" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Desperado" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Bank Job" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Eater" (get-in @state [:runner :play-area])))
    (is (= 4 (count (:hosted (:identity (get-runner))))) "4 cards in NVRAM")
    (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
    (click-prompt state :corp "Keep")
    (click-prompt state :runner "Keep")
    (take-credits state :corp)
    (is (= 2 (count (get-in @state [:runner :hand]))) "There are 2 cards in the runner's Grip")
    (card-ability state :runner (:identity (get-runner)) 0)
    (click-prompt state :runner (find-card "Bank Job" (:hosted (:identity (get-runner)))))
    (is (= 3 (count (get-in @state [:runner :hand]))) "There are 3 cards in the runner's Grip")))

(deftest az-mccaffrey-mechanical-prodigy
  ;; Az McCaffrey: Mechanical Prodigy
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "Az McCaffrey: Mechanical Prodigy"
                          :deck ["Bank Job" "Drug Dealer" "HQ Interface"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (is (= 1 (count (get-resource state))) "One installed resource")
      (is (= 5 (:credit (get-runner))) "Az discount was applied")
      (play-from-hand state :runner "Drug Dealer")
      (is (= 2 (count (get-resource state))) "Two installed resources")
      (is (= 4 (:credit (get-runner))) "Az discount not applied on 2nd install")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [creds (:credit (get-runner))]
        (play-from-hand state :runner "HQ Interface")
        (is (= 1 (count (get-hardware state))) "One installed hardware")
        (is (= (- creds 3) (:credit (get-runner))) "Az discount was applied"))))
  (testing "Test for interaction with Hostage"
    (do-game
      (new-game {:runner {:id "Az McCaffrey: Mechanical Prodigy"
                          :deck ["Hostage" "Professional Contacts"]
                          :hand ["Hostage"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hostage")
      (click-prompt state :runner "Professional Contacts")
      (click-prompt state :runner "Yes")
      (is (= "Professional Contacts" (:title (get-resource state 0))) "ProCo was correctly installed")
      (is (= (+ 5 -1 -5 1) (:credit (get-runner))) "Spent all credits. Was at 5, -1 hostage, -5 ProCo, +1 ID"))))

(deftest azmari-edtech-shaping-the-future
  ;; Azmari EdTech: Shaping the Future
  (testing "Don't gain credits when installing facedown #4477"
    (do-game
      (new-game {:corp {:id "Azmari EdTech: Shaping the Future"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:id "Apex: Invasive Predator"
                          :hand ["Sure Gamble"]}})
      (take-credits state :corp)
      (click-prompt state :corp "Event")
      (core/end-phase-12 state :runner nil)
      (let [credits (:credit (get-corp))]
        (click-card state :runner "Sure Gamble")
        (is (= credits (:credit (get-corp))) "Corp gains no credits from facedown install"))) ))

(deftest blue-sun-powering-the-future
  ;; Blue Sun - Pick up cards at start of turn
  (do-game
    (new-game {:corp {:id "Blue Sun: Powering the Future"
                      :deck [(qty "Hedge Fund" 5)]
                      :hand ["Reduced Service"]}})
    (play-from-hand state :corp "Reduced Service" "New remote")
    (let [rs (get-content state :remote1 0)]
      (core/rez state :corp rs)
      (click-prompt state :corp "3")
      (is (= 3 (get-counters (refresh rs) :power)) "Reduced Service should have 3 counters on it")
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp rs)
      (is (nil? (refresh rs)) "Reduced Service is picked up")
      (is (find-card "Reduced Service" (:hand (get-corp))) "Reduced Service is now in HQ"))
    (play-from-hand state :corp "Reduced Service" "New remote")
    (is (zero? (get-counters (get-content state :remote2 0) :power)) "Reduced Service should have 0 counters on it after reinstall")))

(deftest cerebral-imaging-infinite-frontiers
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game {:corp {:id "Cerebral Imaging: Infinite Frontiers"
                      :deck [(qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Has 13 credits")
    (is (= 13 (hand-size :corp)) "Max hand size is 13")))

(deftest chaos-theory-wunderkind
  ;; Chaos Theory, start with +1 MU
  (do-game
    (new-game {:runner {:id "Chaos Theory: Wünderkind"}})
    (is (= 5 (core/available-mu state)) "Chaos Theory starts the game with +1 MU")))

(deftest chronos-protocol-selective-mind-mapping
  ;; Chronos Protocol - Choose Runner discard for first net damage of a turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :hand [(qty "Neural EMP" 2)]}
                 :runner {:deck [(qty "Imp" 3)]}})
      (take-credits state :corp)
      (core/damage state :corp :net 1)
      (click-prompt state :corp "Yes")
      (let [imp (find-card "Imp" (:hand (get-runner)))]
        (click-prompt state :corp imp)
        (is (= 1 (count (:discard (get-runner)))))
        (core/damage state :corp :net 1)
        (is (empty? (:prompt (get-corp))) "No choice on second net damage")
        (is (= 2 (count (:discard (get-runner)))))
        (run-empty-server state "Archives")
        (take-credits state :runner)
        (core/move state :runner (find-card "Imp" (:discard (get-runner))) :hand)
        (play-from-hand state :corp "Neural EMP")
        (click-prompt state :corp "No")
        (is (= 2 (count (:discard (get-runner)))) "Damage dealt after declining ability")
        (play-from-hand state :corp "Neural EMP")
        (is (empty? (:prompt (get-corp))) "No choice after declining on first damage")
        (is (= 3 (count (:discard (get-runner))))))))
  (testing "with Obokata: Pay 4 net damage to steal. Only 3 damage left after Chronos. No trigger of damage prevent."
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck [(qty "Obokata Protocol" 5)]}
                 :runner {:deck [(qty "Sure Gamble" 3) "Inti" "Feedback Filter"]}})
      (core/gain state :runner :credit 10)
      (play-from-hand state :corp "Obokata Protocol" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay to steal")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp (find-card "Inti" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "Feedback Filter net damage prevention opportunity not given")
      (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 net damage")))
  (testing "vs Employee Strike. Issue #1958"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck ["Pup"]}
                 :runner {:deck ["Employee Strike" (qty "Scrubbed" 3) "Sure Gamble"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (core/damage state :corp :net 1)
      (is (empty? (:prompt (get-corp))) "No choice because of Employee Strike")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Scrubbed")
      (core/damage state :corp :net 1)
      (is (utils/same-card? (:card (prompt-map :corp)) (:identity (get-corp))) "Employee Strike out of play - Ability turned on correctly")))
  (testing "Doesn't prompt when Runner's hand is empty"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Neural EMP"]}
                 :runner {:discard ["Sure Gamble"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Neural EMP")
      (is (empty? (:prompt (get-corp))) "No choice because grip is empty")
      (is (= :corp (:winner @state))))))

(deftest earth-station-sea-headquarters
  ;;Earth Station: SEA Headquarters
  ;;Flipside. Earth Station: Ascending to Orbit
  (testing "Front side:"
    (testing "Additional cost to run HQ"
      (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (take-credits state :corp)
        (changes-val-macro -1 (:credit (get-runner))
                           "Paid 1c to run on HQ"
                           (run-on state :hq))))
    (testing "Flipping costs 1 click"
      (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (changes-val-macro -1 (:click (get-corp))
                           "Paid 1 click to flip Earth Station"
                           (card-ability state :corp (get-in @state [:corp :identity]) 0))
        (is (:flipped (get-in @state [:corp :identity])) "Earth Station is on flip side")
        (is (last-log-contains? state "Corp spends \\[Click\\] to use Earth Station: SEA Headquarters to flip their identity to Earth Station: Ascending to Orbit.") "Should have correct log with click price"))))
  (testing "Flip side:"
    (testing "No additional cost to run HQ"
      (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (take-credits state :corp)
        (changes-val-macro 0 (:credit (get-runner))
                           "Paid nothing to run on HQ"
                           (run-on state :hq))))
    (testing "Can't use ability to flip back to front side"
      (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (is (:flipped (get-in @state [:corp :identity])) "Earth Station is on flip side")
        (changes-val-macro 0 (:click (get-corp))
                           "Paid 1 click to flip Earth Station"
                           (card-ability state :corp (get-in @state [:corp :identity]) 0))
        (is (:flipped (get-in @state [:corp :identity])) "Earth Station is still on flip side")))
    (testing "Additional cost to run a remote"
      (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"
                          :deck ["PAD Campaign"]}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (changes-val-macro -6 (:credit (get-runner))
                           "Paid nothing to run on HQ"
                           (run-on state :remote1))))
    (testing "No additional cost to run HQ"
      (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"
                          :deck ["PAD Campaign"]}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (changes-val-macro 0 (:credit (get-runner))
                           "Paid nothing to run on HQ"
                           (run-on state :hq))))
    (testing "Flip back on successful HQ run"
      (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"
                          :deck ["PAD Campaign"]}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (is (:flipped (get-in @state [:corp :identity])) "Corp ID is on the flip side")
        (changes-val-macro 0 (:credit (get-runner))
                           "Paid nothing to run on HQ"
                           (run-empty-server state :hq))
        (is (not (:flipped (get-in @state [:corp :identity]))) "Corp ID is on the front side"))))
  (testing "Cannot install more than one remote"
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :deck [(qty "PAD Campaign" 2)]}})
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (= 1 (count (core/get-remotes state))) "Could not install second remote")))
  (testing "Creating more servers while the identity is disabled"
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :hand ["PAD Campaign" "NASX" "Project Atlas" (qty "Bio Vault" 2) (qty "Vanilla" 5)]}
                 :runner {:deck ["Employee Strike"]}})
      (core/gain state :corp :click 3)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Vanilla" "Server 1")
      (play-from-hand state :corp "Bio Vault" "Server 1")
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "R&D")
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (play-from-hand state :corp "NASX" "New remote")
      (play-from-hand state :corp "Vanilla" "Server 2")
      (play-from-hand state :corp "Bio Vault" "Server 2")
      (core/gain state :corp :click 4)
      (play-and-score state "Project Atlas")
      (click-prompt state :corp "Server 2")
      (is (= 3 (count (:discard (get-corp)))) "Contents of server 1 were trashed")
      (is (not-empty (find-card "PAD Campaign" (:discard (get-corp)))) "PAD Campaign (was in server 1) was trashed")
      (is (not-empty (get-content state :remote2 0)) "PAD Campaign (was in server 1) was trashed")))
  (testing "Rules corner case: Architects on non-saved remotes can not be trashed"
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :hand [(qty "Architect" 2) "Project Atlas" (qty "PAD Campaign" 2)]}
                 :runner {:deck ["Employee Strike"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (dotimes [_ 2] (play-from-hand state :corp "Architect" "New remote"))
      (core/rez state :corp (get-ice state :remote1 0))
      (core/rez state :corp (get-ice state :remote2 0))
      (play-and-score state "Project Atlas")
      (click-prompt state :corp "Server 2")
      (is (= 0 (count (:discard (get-corp)))) "None of the Architects were trashed")))
  (testing "Worlds Plaza interaction. Issue #4723"
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :hand ["Worlds Plaza" "NASX"]}})
      (play-from-hand state :corp "Worlds Plaza" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (card-ability state :corp (get-content state :remote1 0) 0)
      (click-card state :corp "NASX")
      (is (= "NASX" (:title (first (:hosted (get-content state :remote1 0))))))))
  (testing "Full Immersion RecStudio interaction. Issue #4723"
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :hand ["Full Immersion RecStudio" "NASX"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (card-ability state :corp (get-content state :remote1 0) 0)
      (click-card state :corp "NASX")
      (is (= "NASX" (:title (first (:hosted (get-content state :remote1 0)))))))))

(deftest edward-kim-humanity-s-hammer
  ;; Edward Kim
  (testing "Trash first operation accessed each turn, but not if first one was in Archives"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 2) "PAD Campaign"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck [(qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "PAD Campaign")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "No operation trashed from HQ; accessed one in Archives first")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :hand)
      (is (= 1 (count (:discard (get-corp)))))
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))))
      (is (nil? (get-run)))))
  (testing "Interaction with Eater"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Hedge Fund"]
                        :discard ["Hedge Fund"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Eater" (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Eater")
      (let [eater (get-program state 0)]
        (run-on state "Archives")
        (core/rez state :corp (get-ice state :archives 0))
        (run-continue state)
        (card-ability state :runner eater 0)
        (click-prompt state :runner "End the run")
        (run-continue state)
        (run-continue state)
        (run-successful state)
        (is (= 1 (count (:discard (get-corp)))))
        (run-empty-server state "HQ")
        (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))
  (testing "Do not trigger Maw on first Operation access (due to trash)"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 2)]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Maw" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Maw")
      (is (zero? (count (:discard (get-corp)))) "No cards in Archives")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Only one card trashed from HQ, by Ed Kim")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "One more card trashed from HQ, by Maw")))
  (testing "Do not trigger trash with Divide and Conquer"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3)]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Divide and Conquer" (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (is (= 1 (count (:discard (get-corp)))) "Only Hedge Fund in archives")
      (play-from-hand state :runner "Divide and Conquer")
      (run-continue state)
      (run-successful state)
      (is (= 1 (count (:discard (get-corp)))) "Still only Hedge Fund in archives")))
  (testing "Trashing an operation not during a run won't create a run. Issue #3399"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hostile Takeover"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :hand ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (is (empty? (:prompt (get-corp))))
      (is (empty? (:prompt (get-runner))))
      (is (nil? (get-run)) "No run has been created")))
  (testing "Interaction with Aumakua and accessing an operation in archives #5054"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :discard [(qty "Hedge Fund" 2)]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :hand ["Aumakua"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (get-program state 0) :virus)))))
  (testing "Trashed card is logged"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (last-log-contains? state "Runner uses Edward Kim: Humanity's Hammer to trash Hedge Fund at no cost.")))))

(deftest ele-smoke-scovak-cynosure-of-the-net
  ;; Ele "Smoke" Scovak: Cynosure of the Net
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:id "Ele \"Smoke\" Scovak: Cynosure of the Net"
                          :deck ["Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Refractor")
      (let [smoke (get-in @state [:runner :identity])
            refr (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 1 credit from Smoke"
                           (card-ability state :runner refr 1)
                           (click-card state :runner smoke))))))

(deftest exile-streethawk
  ;; Exile
  (testing "Simultaneous-resolution prompt shown for interaction with Customized Secretary"
    (do-game
      (new-game {:runner {:id "Exile: Streethawk"
                          :deck [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                 (qty "Sure Gamble" 3)]
                          :hand ["Clone Chip"]
                          :discard ["Customized Secretary"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (card-ability state :runner (get-hardware state 0) 0)
      (click-card state :runner (find-card "Customized Secretary" (:discard (get-runner))))
      ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
      (is (= 2 (count (prompt-buttons :runner))) "Simultaneous-resolution prompt is showing")
      (click-prompt state :runner "Exile: Streethawk")
      (is (= 1 (count (:hand (get-runner)))) "Exile drew a card")))
  (testing "Interaction with Harbinger"
    (do-game
      (new-game {:corp {:deck ["Grim"]}
                 :runner {:id "Exile: Streethawk"
                          :deck ["Harbinger" (qty "Sure Gamble" 10)]}})
      (play-from-hand state :corp "Grim" "HQ")
      (take-credits state :corp)
      (starting-hand state :runner ["Harbinger"])
      (play-from-hand state :runner "Harbinger")
      (let [harb (get-program state 0)
            grim (get-ice state :hq 0)]
        (run-on state :hq)
        (core/rez state :corp grim)
        (run-continue state)
        (card-subroutine state :corp (refresh grim) 0)
        (click-card state :corp harb)
        (is (= 0 (count (:hand (get-runner)))) "Exile didn't draw a card")))))

(deftest freedom-khumalo-crypto-anarchist
  ;; Freedom Khumalo - Can spend virus counters from other cards to trash accessed cards with play/rez costs
  (testing "Only works with Assets, ICE, Operations, and Upgrades"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (click-prompt state :runner "[Freedom Khumalo] Trash card")
                (click-card state :runner (get-program state 0))
                (click-card state :runner (get-program state 0))
                (is (= 1 (count (:discard (get-corp))))
                    (str "Accessed " card " should have been trashed after selecting two virus counters"))))]
      (doall (map fk-test
                  ["Dedicated Response Team"
                   "Consulting Visit"
                   "Builder"
                   "Research Station"]))))
  (testing "Triggers when play/rez cost less than or equal to number of available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (let [cost (->> (get-corp) :hand first :cost)]
                  (click-prompt state :runner "[Freedom Khumalo] Trash card")
                  (when (pos? cost)
                    (dotimes [_ cost]
                      (click-card state :runner (get-program state 0))))
                  (is (= 1 (count (:discard (get-corp))))
                      (str "Accessed " card " should have been trashed after selecting " cost " virus counters")))))]
      (doall (map fk-test
                  ["Beanstalk Royalties"
                   "Aggressive Negotiation"
                   "Consulting Visit"
                   "Door to Door"]))))
  (testing "Doesn't trigger when there aren't enough available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (is (= ["No action"] (prompt-buttons :runner)) "Only option should be 'No action'")))]
      (doall (map fk-test
                  ["Archer"
                   "Fire Wall"
                   "Colossus"
                   "Tyrant"]))))
  (testing "Can use multiple programs for virus counter payment"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache" "Virus Breeding Ground"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Virus Breeding Ground")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "[Freedom Khumalo] Trash card")
      (click-card state :runner (get-program state 0))
      (click-card state :runner (get-resource state 0))
      (is (= 1 (count (:discard (get-corp))))
          (str "Accessed Dedicated Response Team should have been trashed after selecting 2 virus counters"))))
  (testing "Can use viruses on hosted cards"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Trypano"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (let [iw (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (refresh iw))
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes")
        (run-empty-server state "HQ")
        (click-prompt state :runner "[Freedom Khumalo] Trash card")
        (click-card state :runner (-> (refresh iw) :hosted first)))
      (is (= 1 (count (:discard (get-corp)))) "Accessed Ice Wall should be discarded after selecting 1 virus counter")))
  (testing "Doesn't trigger when accessing an Agenda"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= ["Steal"] (prompt-buttons :runner)) "Only option should be 'Steal'")))
  (testing "Shows multiple prompts when playing Imp"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Sure Gamble" "Cache" "Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Imp")
      (run-empty-server state "HQ")
      (is (= 4 (count (prompt-buttons :runner))) "Should have 4 options: Freedom, Imp, Trash, No action")))
  (testing "Should return to access prompts when Done is pressed"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= 3 (count (prompt-buttons :runner))) "Should have 3 choices: Freedom, Trash, No action")
      (click-prompt state :runner "[Freedom Khumalo] Trash card")
      (click-card state :runner (get-program state 0))
      (click-prompt state :runner "Done")
      (is (= 3 (count (prompt-buttons :runner)))
          (str "Should go back to access prompts, with 3 choices: Freedom, Trash, No action. "
               "Choices seen: " (prompt-buttons :runner)))
      (click-prompt state :runner "[Freedom Khumalo] Trash card")
      (click-card state :runner (get-program state 0))
      (click-card state :runner (get-program state 0))
      (is (= 1 (count (:discard (get-corp)))) "Card should now be properly discarded")))
  (testing "Shouldn't grant additional accesses after trashing accessed card. #3423"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Ice Wall"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "R&D")
      (click-prompt state :runner "[Freedom Khumalo] Trash card")
      (click-card state :runner (get-program state 0))
      (is (= 1 (count (:discard (get-corp)))) "Accessed Ice Wall should be discarded now")
      (is (not (:run @state)) "Run ended")))
  (testing "Shouldn't give Aumakua additional counters on trash. #3479"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Ice Wall"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache" "Aumakua"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state "R&D")
      (is (zero? (get-counters (get-program state 1) :virus)) "Aumakuma shouldn't have any virus counters yet.")
      (click-prompt state :runner "[Freedom Khumalo] Trash card")
      (click-card state :runner (get-program state 0))
      (is (= 1 (count (:discard (get-corp)))) "Ice Wall should be discarded now")
      (is (zero? (get-counters (get-program state 1) :virus)) "Aumakua doesn't gain any virus counters from trash ability.")
      (is (not (:run @state)) "Run ended")))
  (testing "interaction with trash-cost-bonuses, and declining ability once initiated"
    (do-game
      (new-game {:corp {:deck ["The Board"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Skulljack" "Imp" "Sure Gamble"]}})
      (play-from-hand state :corp "The Board" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 1 (count (prompt-buttons :runner))) "Runner doesn't have enough credits to trash")
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Imp")
      (core/add-counter state :runner (get-program state 0) :virus 5)
      (play-from-hand state :runner "Skulljack")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "The Board should cost 6 to trash")
      (is (= 3 (count (prompt-buttons :runner))) "Runner can use Freedom or Imp to trash")
      (click-prompt state :runner "[Freedom Khumalo] Trash card")
      (click-card state :runner (get-program state 0))
      (click-prompt state :runner "Done")
      (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "Skulljack shouldn't trigger a second time")
      (is (= 3 (count (prompt-buttons :runner))) "Runner can still use Freedom or Imp the second time around")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board with Imp and gain 2 agenda points")))
  (testing "Doesn't trigger when Cerberal Static installed"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Cerebral Static"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (play-from-hand state :corp "Cerebral Static")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= 1 (count (prompt-buttons :runner))) "Should only have 1 option")
      (is (= ["No action"] (prompt-buttons :runner)) "Only option should be 'Done'"))))

(deftest gabriel-santiago-consummate-professional
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game {:runner {:id "Gabriel Santiago: Consummate Professional"
                        :deck ["Easy Mark"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-runner))) "No credits gained")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (is (= 7 (:credit (get-runner))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "No credits gained")))

(deftest gagarin-deep-space-expanding-the-horizon
  ;; Gagarin - pay 1c to access each card in remote
  (do-game
    (new-game {:corp {:id "Gagarin Deep Space: Expanding the Horizon"
                      :deck ["PAD Campaign" "Caprice Nisei"]}})
    (core/lose state :runner :credit 4)
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay to access")
    (is (zero? (:credit (get-runner))) "Paid 1 credit to access")
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :remote1)
    (click-prompt state :runner "OK") ; Could not afford message dismissed
    (is (empty? (:prompt (get-runner))) "Runner cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprice") "Accessed card name was logged")))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (count-bad-pub state)) "GRNDL starts with 1 bad publicity")))
  (testing "vs Valencia - only 1 bad pub at start"
    (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (count-bad-pub state)) "GRNDL starts with 1 bad publicity"))))

(deftest haarpsichord-studios-entertainment-unleashed
  ;; Haarpsichord Studios
  (testing "Prevent stealing more than 1 agenda per turn"
    (do-game
      (new-game {:corp {:id "Haarpsichord Studios: Entertainment Unleashed"
                        :deck [(qty "15 Minutes" 3)]}
                 :runner {:deck ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 1 (:agenda-point (get-runner))) "Second steal of turn prevented")
      (take-credits state :runner)
      (play-from-hand state :corp "15 Minutes" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Steal prevention didn't carry over to Corp turn")))
  (testing "Interactions with Employee Strike. Issue #1313"
    (do-game
      (new-game {:corp {:id "Haarpsichord Studios: Entertainment Unleashed"
                        :deck [(qty "15 Minutes" 3)]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 5)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (play-from-hand state :runner "Employee Strike")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Second steal not prevented")
      (play-from-hand state :runner "Scrubbed")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 2 (:agenda-point (get-runner))) "Third steal prevented"))))

(deftest haas-bioroid-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to rez after passing bioroid
  (do-game
    (new-game {:corp {:id "Haas-Bioroid: Architects of Tomorrow"
                      :deck [(qty "Eli 1.0" 2) "Pup"]}})
    (core/gain state :corp :credit 3)
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (play-from-hand state :corp "Pup" "Archives")
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (take-credits state :corp)
    (run-on state "Archives")
    (core/rez state :corp (get-ice state :archives 1))
    (run-continue state)
    (run-continue state)
    (core/rez state :corp (get-ice state :archives 0))
    (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (run-continue state)
    (click-card state :corp (get-ice state :hq 0))
    (is (= 3 (:credit (get-corp))) "Corp not charged for Architects of Tomorrow rez of Eli 1.0")
    (is (rezzed? (get-ice state :hq 0)) "Eli 1.0 is rezzed")))

(deftest haas-bioroid-engineering-the-future
  ;; Engineering the Future
  (testing "interaction with Employee Strike"
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Engineering the Future"
                        :deck [(qty "Eli 1.0" 3) "Paywall Implementation"]}
                 :runner {:deck ["Employee Strike"]}})
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
      (is (= 9 (:credit (get-corp))) "Corp gained 1cr from EtF"))))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game {:corp {:id "Haas-Bioroid: Stronger Together"
                      :deck ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (core/rez state :corp eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest hayley-kaplan-universal-scholar
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :hand ["Corroder" "Cache" (qty "Fan Site" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Cache" (:hand (get-runner))))
      (is (= 2 (count (:hand (get-runner)))) "Installed Corroder and Cache.")
      (play-from-hand state :runner "Fan Site")
      (is (empty? (:prompt (get-runner))) "No Hayley prompt if not first install this turn.")))
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :hand ["Corroder" "Sahasrara"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sahasrara")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
      (let [rara (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 2 credits from Sahasrara to install Corroder"
                           (click-card state :runner rara)
                           (click-card state :runner rara)))
      (is (empty? (:hand (get-runner))) "Installed Sahasrara and Corroder.")))
  (testing "Fake prompt when nothing to install"
    (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :hand ["Corroder" (qty "Fan Site" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (is (= :waiting (prompt-type :corp)) "Corp has a wait prompt")
      (is (= :bogus (prompt-type :runner)) "Runner has a bogus prompt to fake out the corp")
      (click-prompt state :runner "Carry on!")
      (is (= 2 (count (:hand (get-runner)))) "Installed Corroder and Cache.")
      (play-from-hand state :runner "Fan Site")
      (is (empty? (:prompt (get-corp))) "No Hayley wait prompt if not first install this turn.")
      (is (empty? (:prompt (get-runner))) "No Hayley prompt if not first install this turn.")))
  (testing "Facedown installs do not prompt for Hayley install"
    (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :hand ["Hunting Grounds"]
                          :deck ["Sure Gamble" "Astrolabe" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (click-prompt state :runner "Carry on!")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner (get-resource state 0) 1)
      (is (empty? (:prompt (get-corp))) "No Hayley wait prompt for facedown installs."))))

(deftest hoshiko-shiro-untold-protagonist
  ;; Hoshiko Shiro
  (testing "ID ability"
    (do-game
      (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"
                          :deck [(qty "Sure Gamble" 5)]}})
      (let [ho (get-in @state [:runner :identity])]
        (dotimes [_ 5] (core/move state :runner (first (:hand (get-runner))) :deck))
        (take-credits state :corp)
        (is (not (:flipped (refresh ho))) "Hoshiko starts unflipped")
        (is (= 0 (count (:hand (get-runner)))) "Test starts without cards in grip")
        (take-credits state :runner)
        (is (= 9 (:credit (get-runner))) "No credits from ID ability")
        (is (not (:flipped (refresh ho))) "Hoshiko stays unflipped without access")
        (is (= 0 (count (:hand (get-runner)))) "No draw")
        (take-credits state :corp)
        (run-empty-server state :hq)
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (is (:flipped (refresh ho)) "Hoshiko flips because of access")
        (is (= 14 (:credit (get-runner))) "2 credits from ID ability")
        (is (= 0 (count (:hand (get-runner)))) "No draw")
        (take-credits state :corp)
        (is (= 13 (:credit (get-runner))) "Lost 1 credit from ID ability")
        (is (= 1 (count (:hand (get-runner)))) "Drew 1 card from ID ability")
        (run-empty-server state :hq)
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (is (:flipped (refresh ho)) "Hoshiko stays flipped because of access")
        (take-credits state :corp)
        (is (= 15 (:credit (get-runner))) "Lost 1 credit from ID ability")
        (is (= 2 (count (:hand (get-runner)))) "Drew 1 card from ID ability")
        (take-credits state :runner)
        (is (not (:flipped (refresh ho))) "Hoshiko flips because of no access")
        (is (= 2 (count (:hand (get-runner)))) "Didn't draw card"))))
  (testing "Interaction with Eater"
    (do-game
      (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"
                          :deck [(qty "Sure Gamble" 4) "Eater"]}
                 :corp {:hand ["Vanilla"]
                        :deck [(qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Eater")
      (let [ho (get-in @state [:runner :identity])
            van (get-ice state :rd 0)
            eat (get-program state 0)]
        (is (not (:flipped (refresh ho))) "Hoshiko starts unflipped")
        (run-on state :rd)
        (core/rez state :corp van)
        (run-continue state)
        (card-ability state :runner eat 0)
        (click-prompt state :runner "End the run")
        (run-continue state)
        (run-continue state)
        (run-successful state)
        (take-credits state :runner)
        (is (not (:flipped (refresh ho))) "Hoshiko does not flip"))))
  (testing "Changing link and subtype when flipping"
    (do-game
      (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"}})
      (take-credits state :corp)
      (let [ho (get-in @state [:runner :identity])]
        (is (not (:flipped (refresh ho))) "Hoshiko starts unflipped")
        (is (= 0 (:link (get-runner))) "Hoshiko starts with 0 link")
        (is (has-subtype? (refresh ho) "Natural") "Hoshiko starts with subtype Natural")
        (run-empty-server state :hq)
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (is (:flipped (refresh ho)) "Hoshiko is flipped")
        (is (= 1 (:link (get-runner))) "Hoshiko now has 1 link")
        (is (has-subtype? (refresh ho) "Digital") "Hoshiko now has the subtype Digital"))))
  (testing "Interaction with DreamNet"
    (do-game
      (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"
                          :deck [(qty "Sure Gamble" 5)]
                          :hand ["DreamNet"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DreamNet")
      (let [ho (get-in @state [:runner :identity])]
        (is (not (:flipped (refresh ho))) "Hoshiko starts unflipped")
        (is (= 0 (:link (get-runner))) "Hoshiko starts with 0 link")
        (is (has-subtype? (refresh ho) "Natural") "Hoshiko starts with subtype Natural")
        (let [cards (count (:hand (get-runner)))
              credits (:credit (get-runner))]
          (run-empty-server state :hq)
          (is (= (inc cards) (count (:hand (get-runner)))) "Runner has drawn 1 card on successful run")
          (is (= credits (:credit (get-runner))) "Runner has gained 0 credits on successful run"))
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (:flipped (refresh ho)) "Hoshiko is flipped")
        (is (= 1 (:link (get-runner))) "Hoshiko now has 1 link")
        (is (has-subtype? (refresh ho) "Digital") "Hoshiko now has the subtype Digital")
        (let [cards (count (:hand (get-runner)))
              credits (:credit (get-runner))]
          (run-empty-server state :hq)
          (is (= (inc cards) (count (:hand (get-runner)))) "Runner has drawn 1 card on successful run")
          (is (= (inc credits) (:credit (get-runner))) "Runner has gained 1 credit on successful run"))))))

(deftest hyoubu-institute-absolute-clarity
  (testing "ID abilities"
    (do-game
     (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"}
                :runner {:deck [(qty "Sure Gamble" 1)]
                         :hand [(qty "Sure Gamble" 2)]}})
     (let [hy (get-in @state [:corp :identity])]
       (dotimes [i 2]
         (is (changes-credits (get-corp) 1 (card-ability state :corp hy i))) ; triggering ID ability triggers a reveal so gains a cred
         (is (changes-credits (get-corp) 0 (card-ability state :corp hy i))) ; triggering ID ability second time in a turn doesn't
         (is (= 1 (:click (get-corp))) "Using ability twice cost 2 clicks")
         (take-credits state :corp)
         (take-credits state :runner))
       (core/move state :runner (first (:deck (get-runner))) :hand)
       (is (= 0 (count (:deck (get-runner)))) "Runner deck is empty")
       (is (changes-credits (get-corp) 0 (card-ability state :corp hy 0)))  ; stack is empty, so revealing nothing gains nothing
       (take-credits state :corp)
       (dotimes [_ 3]
         (play-from-hand state :runner "Sure Gamble"))
       (take-credits state :runner)
       (is (= 0 (count (:hand (get-runner)))) "Runner hand is empty")
       (is (changes-credits (get-corp) 0 (card-ability state :corp hy 1))))))  ; grip is empty, so revealing nothing gains nothing
  (testing "EStrike interaction"
    (do-game
     (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                       :deck ["Scarcity of Resources" "Celebrity Gift"]}
                :runner {:deck [(qty "Employee Strike" 3) "Enhanced Vision"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Enhanced Vision")
     (is (changes-credits (get-corp) 1
                          (run-empty-server state "Archives")))
     (take-credits state :runner)
     (take-credits state :corp)
     (play-from-hand state :runner "Employee Strike")
     (is (changes-credits (get-corp) 0
                          (run-empty-server state "Archives")))
     (take-credits state :runner)
     (core/gain state :corp :click 3)
     (play-from-hand state :corp "Celebrity Gift")
     (is (changes-credits (get-corp) 2 ; get 2 creds from celeb gift, but nothing from hyoubu trigger due to estrike
                          (do (click-card state :corp (first (:hand (get-corp))))
                              (click-prompt state :corp "Done"))))
     (play-from-hand state :corp "Scarcity of Resources")
     (is (changes-credits (get-corp) 0 (card-ability state :corp (get-in @state [:corp :identity]) 1))))) ; hyoubu doesn't mistake the first reveal it sees for first reveal of turn
  (testing "Slot Machine, grail, Reflection, Fast Track"
    (do-game
     (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                       :deck ["Slot Machine" (qty "Galahad" 2) "Fast Track" "House of Knives"]}
                :runner {:deck ["Reflection"]}})
     (core/gain state :corp :credit 20)
     (play-from-hand state :corp "Galahad" "HQ")
     (play-from-hand state :corp "Slot Machine" "R&D")
     (let [gal (get-ice state :hq 0)
           sm (get-ice state :rd 0)]
       (take-credits state :corp)
       (play-from-hand state :runner "Reflection")
       (run-on state "HQ")
       (core/rez state :corp gal)
       (run-continue state)
       (card-ability state :corp (refresh gal) 0) ;reveal grail ice
       (click-card state :corp (find-card "Galahad" (:hand (get-corp))))
       (is (changes-credits (get-corp) 1
                            (click-prompt state :corp "Done")))
       (is (changes-credits (get-corp) 0
                            (run-jack-out state))) ; triggers reflection, but trigger already done this turn
       (take-credits state :runner)
       (take-credits state :corp)
       (run-on state "R&D")
       (core/rez state :corp sm)
       (is (changes-credits (get-corp) 1
                            (run-continue state))) ;trigger slot machine
       (run-jack-out state)
       (take-credits state :runner)
       (take-credits state :corp)
       (run-on state "Archives")
       (run-continue state)
       (is (changes-credits (get-corp) 1
                            (run-jack-out state))) ; triggers reflection
       (take-credits state :runner)
       (core/move state :corp (find-card "House of Knives" (:hand (get-corp))) :deck)
       (play-from-hand state :corp "Fast Track")
       (is (changes-credits (get-corp) 1
                            (click-prompt state :corp "House of Knives")))))))

(deftest iain-stirling-retired-spook
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game {:corp {:deck ["Breaking News"]}
               :runner {:id "Iain Stirling: Retired Spook"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])]
      (score-agenda state :corp (refresh ag1))
      (take-credits state :corp)
      (is (= 1 (:agenda-point (get-corp))) "Corp gains 1 agenda point from Breaking News")
      (take-credits state :runner 1)
      (is (= 8 (:credit (get-runner))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-growing-solutions
  ;; Industrial Genomics - Increase trash cost
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck [(qty "PAD Campaign" 3) (qty "Hedge Fund" 3)]}})
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
  (testing "with Product Recall"
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck ["Product Recall" (qty "PAD Campaign" 3) (qty "Hedge Fund" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (let [pad (get-content state :remote1 0)]
        (core/rez state :corp pad)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (= 8 (core/trash-cost state :runner (refresh pad))))
        (run-jack-out state)
        (take-credits state :runner)
        (play-from-hand state :corp "Product Recall")
        (let [credits (:credit (get-corp))]
          (click-card state :corp pad)
          (is (= (+ credits 8) (:credit (get-corp))) "Gain 8 credits from trashing PAD Campaign"))))))

(deftest jemison-astronautics-sacrifice-audacity-success
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Enforcer 1.0" "Hostile Takeover" "Ice Wall" "Global Food Initiative"]}
                 :runner {:deck ["Data Dealer"]}})
      (play-from-hand state :corp "Enforcer 1.0" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (let [enf (get-ice state :hq 0)
            iwall (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Data Dealer")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (let [dd (get-resource state 0)]
          (card-ability state :runner dd 0)
          (click-card state :runner (get-in (get-runner) [:scored 0]))
          (is (empty? (:prompt (get-corp))) "No Jemison prompt for Runner forfeit")
          (take-credits state :runner)
          (play-from-hand state :corp "Global Food Initiative" "New remote")
          (score-agenda state :corp (get-content state :remote2 0))
          (core/rez state :corp enf)
          (click-card state :corp (get-in (get-corp) [:scored 0]))
          (click-card state :corp iwall)
          (is (= 4 (get-counters (refresh iwall) :advancement)) "Jemison placed 4 advancements")))))
  (testing "24/7 - Armed Intimidation combination"
    ;; Expected result: 24/7 causes Forfeit, Jemison places counters, AI triggers
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Armed Intimidation" "Hostile Takeover"
                               "24/7 News Cycle" "Ice Wall"]}})
      (play-and-score state "Hostile Takeover")
      (is (= 1 (:agenda-point (get-corp))) "Corp has 1 agenda points from Hostile Takeover")
      (is (= 12 (:credit (get-corp))) "Corp has 12 credits after scoring Hostile Takeover with play-score")
      (play-and-score state "Armed Intimidation")
      (click-prompt state :runner "Take 2 tags")
      (is (= 3 (:agenda-point (get-corp))) "Corp has 3 agenda points from HT + Armed Intimidation")
      (is (= 2 (count-tags state)) "Runner took 2 tags from AI")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "24/7 News Cycle")
      (click-card state :corp (get-scored state :corp 0)) ; select HT to forfeit
      (let [ice-wall (get-ice state :hq 0)]
        (click-card state :corp ice-wall) ; The Jemison forfeit triggers
        (is (= 2 (get-counters (refresh ice-wall) :advancement)) "Ice Wall has 2 advancement counters from HT forfeit"))
      (click-card state :corp (get-scored state :corp 0)) ; select AI to trigger
      (click-prompt state :runner "Take 2 tags") ; First runner has prompt
      (is (= 4 (count-tags state)) "Runner took 2 more tags from AI -- happens at the end of all the async completion"))))

(deftest jesminder-sareen-girl-behind-the-curtain
  ;; Jesminder Sareen - avoid tags only during a run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["SEA Source" (qty "Data Raven" 2)]
                        :credits 10}
                 :runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Data Raven" "Archives")
      (play-from-hand state :corp "Data Raven" "Archives")
      (take-credits state :corp)
      (let [dr1 (get-ice state :archives 0)
            dr2 (get-ice state :archives 1)]
        (run-on state "Archives")
        (core/rez state :corp dr2)
        (run-continue state)
        (click-prompt state :runner "Take 1 tag")
        (is (zero? (count-tags state)) "Jesminder avoided first tag during the run")
        (run-continue state)
        (core/rez state :corp dr1)
        (run-continue state)
        (click-prompt state :runner "Take 1 tag")
        (is (= 1 (count-tags state)) "Jesminder did not avoid the second tag during the run")
        (core/continue state :corp nil)
        (core/continue state :runner nil)
        (core/continue state :corp nil)
        (core/successful-run state :runner nil)
        (run-empty-server state "R&D") ; clear per-run buffer
        (take-credits state :runner)
        (play-from-hand state :corp "SEA Source")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (count-tags state)) "Jesminder did not avoid the tag outside of a run"))))
  (testing "don't avoid John Masanori tag"
    (do-game
      (new-game {:runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :deck ["John Masanori"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "John Masanori")
      (run-on state "HQ")
      (run-continue state)
      (run-jack-out state)
      (is (= 1 (count-tags state)) "Jesminder did not avoid John Masanori tag"))))

(deftest jinteki-biotech-life-imagined
  ;; Jinteki Biotech
  (testing "Brewery net damage"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Brewery")
      (core/start-turn state :corp nil)
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (= 1 (count (:hand (get-runner)))) "Runner took 2 net damage from Brewery flip")))
  (testing "Greenhouse four advancement tokens"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Greenhouse")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (let [bt (get-content state :remote1 0)]
        (is (zero? (get-counters (refresh bt) :advancement)) "No advancement counters on agenda")
        (card-ability state :corp (:identity (get-corp)) 1)
        (click-card state :corp (refresh bt))
        (is (= 4 (get-counters (refresh bt) :advancement)) "Four advancement counters on agenda"))))
  (testing "Tank shuffle Archives into R&D"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck [(qty "Hedge Fund" 3)]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Tank")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :runner)
      (is (= 3 (count (:discard (get-corp)))) "Archives started with 3 cards")
      (is (zero? (count (:deck (get-corp)))) "R&D started empty")
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (zero? (count (:discard (get-corp)))) "Archives ended empty")
      (is (= 3 (count (:deck (get-corp)))) "R&D ended with 3 cards"))))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Take 1 net when an agenda is scored or stolen
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Braintrust" 6)]}
                 :runner {:hand [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage from steal")))
  (testing "Interaction with Employee Striek. Issue #4124"
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Braintrust" 6)]}
                 :runner {:hand ["Employee Strike" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 3 (count (:hand (get-runner)))) "Runner took 0 net damage from steal"))))

(deftest jinteki-potential-unleashed
  ;; Potential Unleashed - when the runner takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game {:corp {:id "Jinteki: Potential Unleashed"
                      :deck ["Philotic Entanglement" "Neural EMP" (qty "Braintrust" 3)]}
               :runner {:deck [(qty "Employee Strike" 10)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Philotic Entanglement" "New remote")
    (score-agenda state :corp (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-runner)))))
    (play-from-hand state :corp "Neural EMP")
    (is (= 5 (count (:discard (get-runner)))))))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent runner from running on remotes unless they first run on a central
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]}})
      (play-from-hand state :corp "Mental Health Clinic" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (run-empty-server state "HQ")
      (is (core/can-run-server? state "Server 1") "Runner can run on remotes")))
  (testing "interaction with Employee Strike. Issue #1313 and #1956."
    (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (play-from-hand state :corp "Mental Health Clinic" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (play-from-hand state :runner "Employee Strike")
      (is (core/can-run-server? state "Server 1") "Runner can run on remotes")
      (play-from-hand state :runner "Scrubbed")
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals"))))

(deftest kabonesa-wu-netspace-thrillseeker
  ;; Kabonesa Wu
  (testing "Basic test"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:id "Kabonesa Wu: Netspace Thrillseeker"
                          :deck ["Cache" "Gordian Blade"]
                          :hand ["Sure Gamble"]}})
      (card-ability state :runner (:identity (get-runner)) 0)
      (is (= [(find-card "Gordian Blade" (:deck (get-runner))) "Cancel"]
             (map :value (:choices (prompt-map :runner))))
          "Cache shouldn't be in the prompt")
      (click-prompt state :runner "Gordian Blade")
      (is (some? (get-program state 0)) "Gordian Blade should be installed")
      (is (= 2 (:credit (get-runner))) "Runner only spends 3 for Gordian Blade")
      (take-credits state :runner)
      (is (nil? (get-program state 0)) "Gordian Blade shouldn't be installed anymore")
      (is (= "Gordian Blade" (-> (get-runner) :rfg last :title)) "Kabonesa Wu should rfg card installed with ability")))
  (testing "Basic test"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:id "Kabonesa Wu: Netspace Thrillseeker"
                          :deck ["Cache" "Gordian Blade"]
                          :hand ["Sure Gamble" "Rebirth"]}})
      (card-ability state :runner (:identity (get-runner)) 0)
      (is (= [(find-card "Gordian Blade" (:deck (get-runner))) "Cancel"] (prompt-buttons :runner))
          "Cache shouldn't be in the prompt")
      (click-prompt state :runner "Gordian Blade")
      (is (some? (get-program state 0)) "Gordian Blade should be installed")
      (play-from-hand state :runner "Rebirth")
      (click-prompt state :runner "Lat: Ethical Freelancer")
      (is (= "Lat: Ethical Freelancer" (:title (:identity (get-runner)))) "Runner is now Lat")
      (take-credits state :runner)
      (is (nil? (get-program state 0)) "Gordian Blade shouldn't be installed anymore")
      (is (= "Gordian Blade" (-> (get-runner) :rfg last :title))
          "Kabonesa Wu should rfg card installed with ability even tho runner is now a different identity"))))

(deftest kate-mac-mccaffrey-digital-tinker
  ;; Kate 'Mac' McCaffrey
  (testing "Install discount"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))
  (testing "No discount for 0 cost"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"
                                 "Self-modifying Code"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Magnum Opus")
      (is (zero? (:credit (get-runner))) "No Kate discount on second program install")))
  (testing "Can afford only with the discount"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (core/lose state :runner :credit 1)
      (is (= 4 (:credit (get-runner))))
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (count (get-program state))) "Magnum Opus installed")
      (is (zero? (:credit (get-runner))) "Installed Magnum Opus for 4 credits"))))

(deftest ken-express-tenma-disappeared-clone
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game {:runner {:id "Ken \"Express\" Tenma: Disappeared Clone"
                        :deck [(qty "Account Siphon" 2)]}})
    (take-credits state :corp)
    (play-run-event state "Account Siphon" :hq)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit for first Run event")
    (click-prompt state :runner "Account Siphon")
    (play-run-event state "Account Siphon" :hq)
    (is (= 16 (:credit (get-runner))) "No credit gained for second Run event")))

(deftest khan-savvy-skiptracer
  ;; Khan
  (testing "proper order of events when vs. Caprice"
    (do-game
      (new-game {:corp {:deck ["Eli 1.0" "Caprice Nisei"]}
                 :runner {:id "Khan: Savvy Skiptracer"
                          :deck ["Corroder"]}})
      (play-from-hand state :corp "Eli 1.0" "Archives")
      (play-from-hand state :corp "Caprice Nisei" "Archives")
      (core/rez state :corp (get-content state :archives 0))
      (take-credits state :corp)
      (run-on state "Archives")
      (run-continue state)
      (is (and (= 1 (count (:prompt (get-runner))))
               (= "Khan: Savvy Skiptracer" (-> (prompt-map :runner) :card :title)))
          "Only Khan prompt showing")
      (click-card state :runner "Corroder")
      (is (find-card "Corroder" (-> (get-runner) :rig :program)) "Corroder installed")
      (is (= 4 (:credit (get-runner))) "1cr discount from Khan")
      (is (= "Caprice Nisei" (-> (prompt-map :runner) :card :title)) "Caprice prompt showing")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest laramy-fisk-savvy-investor
  ;; Laramy Fisk
  (testing "installing a Shard should still give option to force Corp draw"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]
                        :hand [(qty "Hedge Fund" 2) (qty "Eli 1.0" 3)]}
                 :runner {:id "Laramy Fisk: Savvy Investor"
                          :deck ["Eden Shard"]}})
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Eden Shard")
      (click-prompt state :runner "Yes") ; Eden Shard optional, is a replacement effect
      (is (= "Identity" (-> (prompt-map :runner) :card :type)) "Fisk prompt showing")
      (click-prompt state :runner "Yes") ; Fisk optional
      (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
      (is (= 5 (:credit (get-runner))) "Eden Shard install was free")
      (is (not (:run @state)) "Run ended")
      (is (= 6 (count (:hand (get-corp)))) "Corp forced to draw"))))

(deftest lat-ethical-freelancer
  ;; Lat: Ethical Freelancer
  (testing "Ability fires - draw"
    (do-game
      (new-game {:runner {:id "Lat: Ethical Freelancer"
                          :deck [(qty "Sure Gamble" 6)]}
                 :corp {:deck [(qty "Hedge Fund" 5)]}
                 :options {:start-as :runner}})
      (core/lose state :runner :click 4)
      (core/end-turn state :runner nil)
      (is (= "Draw 1 card?" (:msg (prompt-map :runner))))
      (is (= 5 (count (:hand (get-runner)))))
      (click-prompt state :runner "Yes")
      (is (= 6 (count (:hand (get-runner)))))))
  (testing "Ability fires - don't draw"
    (do-game
      (new-game {:runner {:id "Lat: Ethical Freelancer"
                          :deck [(qty "Sure Gamble" 6)]}
                 :corp {:deck [(qty "Hedge Fund" 5)]}
                 :options {:start-as :runner}})
      (core/lose state :runner :click 4)
      (core/end-turn state :runner nil)
      (is (= "Draw 1 card?" (:msg (prompt-map :runner))))
      (is (= 5 (count (:hand (get-runner)))))
      (click-prompt state :runner "No")
      (is (= 5 (count (:hand (get-runner)))))))
  (testing "Ability doesn't fire"
    (do-game
      (new-game {:runner {:id "Lat: Ethical Freelancer"
                          :deck [(qty "Sure Gamble" 3)]}
                 :corp {:deck [(qty "Hedge Fund" 4)]}
                 :options {:start-as :runner}})
      (core/lose state :runner :click 4)
      (core/end-turn state :runner nil)
      (is (empty? (:prompt (get-runner))) "No prompt"))))

(deftest leela-patel-trained-pragmatist
  ;; Leela Patel
  (testing "complicated interaction with mutiple Gang Sign"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"
                               "Hostile Takeover"
                               "Geothermal Fracking"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck [(qty "Gang Sign" 2)]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Geothermal Fracking" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Leela Patel: Trained Pragmatist")
      (click-card state :runner (get-content state :remote2 0))
      (is (find-card "Hostile Takeover" (:hand (get-corp))) "Hostile Takeover returned to hand")
      (click-prompt state :runner "Gang Sign")
      (click-prompt state :runner "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-runner))) "Hostile Takeover stolen with Gang Sign")
      (click-card state :runner (get-content state :remote3 0))
      (is (find-card "Geothermal Fracking" (:hand (get-corp))) "Geothermal Fracking returned to hand")
      (click-prompt state :runner "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-runner))) "Geothermal Fracking stolen with Gang Sign")
      (click-prompt state :runner "Done")))
  (testing "issues with lingering successful run prompt"
    (do-game
      (new-game {:corp {:id "NBN: Making News"
                        :deck ["Breaking News" "SanSan City Grid"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"}})
      (starting-hand state :corp ["SanSan City Grid"])
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :remote1 0))
      (is (not (:run @state)) "Run is over")))
  (testing "upgrades returned to hand in the middle of a run do not break the run. Issue #2008"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) "Shock!"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck ["Sure Gamble"]}})
      (starting-hand state :corp ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (trash-from-hand state :corp "Project Atlas")
      (trash-from-hand state :corp "Shock!")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :hq 0))
      (is (not (get-content state :hq 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :rd 0))
      (is (not (get-content state :rd 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-server state "Archives")
      (click-prompt state :runner "Shock!")
      (click-prompt state :runner "Project Atlas")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :archives 0))
      (is (not (get-content state :archives 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")))
  (testing ""
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"
                               "Hostile Takeover"
                               "Geothermal Fracking"
                               "Merger"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck ["Gang Sign"]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Geothermal Fracking" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (score-agenda state :corp (get-content state :remote1 0))
      ;; Simultaneous prompt: Leela or Gang Sign
      (is (= ["Leela Patel: Trained Pragmatist" "Gang Sign"] (map :title (prompt-buttons :runner))))
      (click-prompt state :runner "Gang Sign")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :remote2 0)) ; Bounce from Gang Sign steal
      (click-card state :runner (get-content state :remote3 0)) ; Bounce from Hostile score
      (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand now"))))

(deftest liza-talking-thunder-prominent-legislator
  ;; Liza Talking Thunder: Prominent Legislator
  (do-game
    (new-game {:runner {:id "Liza Talking Thunder: Prominent Legislator"
                        :deck [(qty "Sure Gamble" 7)]}})
    (take-credits state :corp)
    (run-empty-server state "R&D")
    (is (= 7 (count (:hand (get-runner)))) "Drew 2 cards from successful run on Archives")
    (is (= 1 (count-tags state)) "Took 1 tag from successful run on Archives")))

(deftest maxx-maximum-punk-rock
  ;; MaxX
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Wyldside" 3)
                                 "Eater"]}})
      (starting-hand state :runner ["Eater"])
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (last-log-contains? state "Wyldside, Wyldside")
          "Maxx did log trashed card names")))
  (testing "with Dummy Box. Check that mills don't trigger trash prevention #3246"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Dummy Box" 30)]}})
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (play-from-hand state :runner "Dummy Box")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (empty? (:prompt (get-runner))) "Dummy Box not fired from mill")))
  (testing "with Wyldside - using Wyldside during Step 1.2 should lose 1 click"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :hand ["Wyldside"]
                          :deck [(qty "Sure Gamble" 10)]}})
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (starting-hand state :runner ["Wyldside"])
      (play-from-hand state :runner "Wyldside")
      (take-credits state :runner)
      (is (= 5 (:credit (get-runner))) "Runner has 5 credits at end of first turn")
      (is (find-card "Wyldside" (get-resource state)) "Wyldside was installed")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
      (is (:runner-phase-12 @state) "Runner is in Step 1.2")
      (let [maxx (get-in @state [:runner :identity])
            wyld (find-card "Wyldside" (get-resource state))]
        (card-ability state :runner wyld 0)
        (card-ability state :runner maxx 0)
        (core/end-phase-12 state :runner nil)
        (is (= 4 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
        (is (= 3 (:click (get-runner))) "Wyldside caused 1 click to be lost")
        (is (= 3 (count (:hand (get-runner)))) "3 cards drawn total")))))

(deftest mirrormorph-endless-iteration
  ;; MirrorMorph: Endless Iteration
  (testing "Mirrormorph triggers on three different actions"
    (testing "Gain credit from MM"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Hedge Fund" 10)]}})
        (core/click-draw state :corp nil)
        (core/click-credit state :corp nil)
        (play-from-hand state :corp "Hedge Fund")
        (changes-val-macro 1 (:credit (get-corp))
                           "Gained 1 credit from MM ability"
                           (click-prompt state :corp "Gain 1 [Credits]"))))
    (testing "Gain click from using Asset ability"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Capital Investors" 10)]}})
        (core/click-draw state :corp nil)
        (play-from-hand state :corp "Capital Investors" "New remote")
        (let [ci (get-content state :remote1 0)
              mm (get-in @state [:corp :identity])]
          (core/rez state :corp ci)
          (card-ability state :corp (refresh ci) 0)
          (click-prompt state :corp "Gain [Click]")
          (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
          (card-ability state :corp (refresh ci) 0)
          (is (= 1 (:click (get-corp))) "Could not use Capital Investors again with MM click")
          (core/click-credit state :corp nil)
          (is (= 0 (:click (get-corp))) "Was able to click for credit"))))
    (testing "Gain click from using Upgrade ability"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Cold Site Server" 10)]}})
        (core/click-draw state :corp nil)
        (play-from-hand state :corp "Cold Site Server" "New remote")
        (let [css (get-content state :remote1 0)
              mm (get-in @state [:corp :identity])]
          (core/rez state :corp css)
          (card-ability state :corp (refresh css) 0)
          (click-prompt state :corp "Gain [Click]")
          (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
          (card-ability state :corp (refresh css) 0)
          (is (= 1 (:click (get-corp))) "Could not use Hedge Fund again with MM click")
          (core/click-credit state :corp nil)
          (is (= 0 (:click (get-corp))) "Was able to click for credit"))))
    (testing "Gain click from playing an Operation"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Hedge Fund" 10)]}})
        (core/click-draw state :corp nil)
        (core/click-credit state :corp nil)
        (play-from-hand state :corp "Hedge Fund")
        (click-prompt state :corp "Gain [Click]")
        (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
        (play-from-hand state :corp "Hedge Fund")
        (is (= 1 (:click (get-corp))) "Could not use Hedge Fund again with MM click")))
    (testing "Gain click from installing card"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "PAD Campaign" 10)]}})
        (core/click-draw state :corp nil)
        (core/click-credit state :corp nil)
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (click-prompt state :corp "Gain [Click]")
        (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (is (= 1 (:click (get-corp))) "Could not install another card with MM click")))
    (testing "Gain click from trashing three different PAD Taps"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :credits 9}
                   :runner {:deck [(qty "PAD Tap" 3)]}})
        (take-credits state :corp)
        (dotimes [_ 3] (play-from-hand state :runner "PAD Tap"))
        (take-credits state :runner)
        (let [tap1 (get-resource state 0)
              tap2 (get-resource state 1)
              tap3 (get-resource state 2)
              mm (get-in @state [:corp :identity])]
          (card-side-ability state :corp tap1 0)
          (card-side-ability state :corp tap2 0)
          (card-side-ability state :corp tap3 0)
          (click-prompt state :corp "Gain [Click]")
          (is (= 1 (:click (get-corp))) "Gained 1 click from MM"))))
    (testing "Trigger Mirrormorph with double Operation"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :hand ["Mandatory Upgrades" "Blue Level Clearance"]
                          :deck [(qty "Hedge Fund" 10)]}})
        (play-and-score state "Mandatory Upgrades")
        (take-credits state :corp)
        (take-credits state :runner)
        (core/click-credit state :corp nil)
        (core/click-draw state :corp nil)
        (play-from-hand state :corp "Blue Level Clearance")
        (changes-val-macro 1 (:credit (get-corp))
                           "Gained 1 credit from MM ability"
                           (click-prompt state :corp "Gain 1 [Credits]"))))
    (testing "Trigger Mirrormorph with MCAAP"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :hand ["MCA Austerity Policy"]
                          :deck [(qty "Hedge Fund" 10)]}})
        (play-from-hand state :corp "MCA Austerity Policy" "New remote")
        (let [mcaap (get-content state :remote1 0)
              mm (get-in @state [:corp :identity])]
          (core/rez state :corp mcaap)
          (card-ability state :corp mcaap 0)
          (dotimes [_ 2]
            (take-credits state :corp)
            (take-credits state :runner)
            (card-ability state :corp mcaap 0))
          (core/click-credit state :corp nil)
          (card-ability state :corp mcaap 1)
          (changes-val-macro 1 (:credit (get-corp))
                             "Gained 1 credit from MM ability"
                             (click-prompt state :corp "Gain 1 [Credits]"))))))
  (testing "Cases where Mirrormorph does not trigger"
    (testing "Using same Asset ability multiple times"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Capital Investors" 10)]}})
        (play-from-hand state :corp "Capital Investors" "New remote")
        (let [ci (get-content state :remote1 0)
              mm (get-in @state [:corp :identity])]
          (core/rez state :corp ci)
          (dotimes [_ 2] (card-ability state :corp (refresh ci) 0))
          (is (empty? (:prompt (get-corp))) "No MM trigger"))))
    (testing "Using different operations"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Hedge Fund" 10)]}})
        (dotimes [_ 3] (play-from-hand state :corp "Hedge Fund"))
        (is (empty? (:prompt (get-corp))) "No MM trigger")))
    (testing "Installing different cards"
      (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :hand ["PAD Campaign" "NASX" "Wall To Wall"]}})
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (play-from-hand state :corp "NASX" "New remote")
        (play-from-hand state :corp "Wall To Wall" "New remote")
        (is (empty? (:prompt (get-corp))) "No MM trigger")))))

(deftest mti-mwekundu-life-improved
  ;; Mti Mwekundu: Life Improved - when server is approached, install ice from HQ at the innermost position
  (testing "No ice"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))
  (testing "Multiple ice"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma" "Ice Wall" "Bloom"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Bloom" "R&D")
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (run-continue state)
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))
  (testing "with Kakugo, passing shouldn't fire net damage twice. #3588"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Kakugo"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Kakugo" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Kakugo" (:title (get-ice state :hq 0))) "Kakugo was installed")
      (is (empty? (:hand (get-corp))) "Kakugo removed from HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (= 1 (-> (get-runner) :discard count)) "Runner should take 1 net damage from Kakugo"))))

(deftest nasir-meidan-cyber-explorer
  ;; Nasir
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [iwall (get-ice state :hq 0)
            nasir (get-in @state [:runner :identity])]
        (is (= 5 (:credit (get-runner))))
        (core/rez state :corp iwall)
        (run-continue state)
        (is (= 1 (:credit (get-runner))) "Credits at 1 after Nasir ability trigger"))))
  (testing "with Xanadu"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"
                          :deck ["Xanadu"]
                          :credits 6}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Xanadu")
      (run-on state "HQ")
      (let [iwall (get-ice state :hq 0)
            nasir (get-in @state [:runner :identity])]
        (is (= 3 (:credit (get-runner))) "Pay 3 to install Xanadu")
        (core/rez state :corp iwall)
        (run-continue state)
        (is (= 2 (:credit (get-runner))) "Gain 1 more credit due to Xanadu")))))

(deftest nathaniel-gnat-hall-one-of-a-kind
  ;; Nathaniel "Gnat" Hall: One-of-a-Kind
  (do-game
    (new-game {:runner {:id "Nathaniel \"Gnat\" Hall: One-of-a-Kind"
                        :deck [(qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Did not gain a credit when Gnat is on 3 cards")
    (play-from-hand state :runner "Sure Gamble")
    (take-credits state :runner)
    (let [runner-credits (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (inc runner-credits) (:credit (get-runner)))) "Gained 1 credits when on 2 cards")))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message
  (testing "Trace to tag Runner when first installed Corp card is trashed. Issue #2321"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck [(qty "Launch Campaign" 3)]}
                 :runner {:deck ["Forger"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Forger")
      ; trash from HQ first - #2321
      (run-empty-server state "HQ")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (empty? (:prompt (get-runner))) "Forger can't avoid the tag")
      (is (= 1 (count-tags state)) "Runner took 1 unpreventable tag")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (empty? (:prompt (get-corp))) "No trace chance on 2nd trashed card of turn")))
  (testing "Interaction with Dedicated Response Team"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck ["Launch Campaign" "Dedicated Response Team"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner took 1 unpreventable tag")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 0 meat damage from DRT cuz it's trashed")))
  (testing "Trace shouldn't fire on second trash after trash during Direct Access run. #4168"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Launch Campaign" 3)]}
                 :runner {:deck ["Direct Access"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Direct Access")
      (click-prompt state :runner "Server 1")
      (run-continue state)
      (run-successful state)
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :runner "Yes")
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (empty? (:prompt (get-corp))) "CtM shouldn't fire")
      (is (empty? (:prompt (get-runner))) "Runner shouldn't have prompt")
      (is (zero? (count-tags state)) "Runner took 1 unpreventable tag")))
 (testing "Trace should fire on first trash of a Corp card after a Runner card is trashed"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Launch Campaign" 3)]}
                 :runner {:deck ["Sure Gamble"]
                          :hand ["Crowdfunding"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Crowdfunding")
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))))
      (core/end-phase-12 state :runner nil)
      (is (zero? (count (get-resource state))))
      (is (= 1 (count (:hand (get-runner)))))
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (seq (:prompt (get-corp))) "Corp should have a Trace prompt")
      (click-prompt state :corp "No"))))

(deftest nbn-making-news
  ;; NBN: Making News
  (testing "Pay credits, and not refilling on disabled. Issue #2439"
    (do-game
      (new-game {:corp {:id "NBN: Making News"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Snatch and Grab" "Scarcity of Resources"]}
                 :runner {:hand ["Employee Strike"]}})
      (let [nbn-mn (:identity (get-corp))]
        (play-from-hand state :corp "Snatch and Grab")
        (is (= (+ (:credit (get-corp)) (get-counters (refresh nbn-mn) :recurring))
               (:choices (prompt-map :corp))) "7 total available credits for the trace")
        (click-prompt state :corp "7")
        (click-card state :corp nbn-mn)
        (click-card state :corp nbn-mn)
        (is (zero? (get-counters (refresh nbn-mn) :recurring)) "Has used recurring credit")
        (is (= 10 (:strength (prompt-map :runner))) "Current trace strength should be 10")
        (click-prompt state :runner "0")
        (click-prompt state :corp "Done")
        (take-credits state :corp)
        (play-from-hand state :runner "Employee Strike")
        (take-credits state :runner)
        (is (zero? (get-counters (refresh nbn-mn) :recurring)) "Making News doesn't get recurring credits cuz it's disabled")
        (play-from-hand state :corp "Scarcity of Resources")
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 2 (get-counters (refresh nbn-mn) :recurring)) "Recurring credits refill once MN isn't disabled anymore")))))

(deftest nero-severn-information-broker
  ;; Nero Severn: Information Broker
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "Nero Severn: Information Broker"
                          :deck [(qty "Sure Gamble" 10)]}
                :corp { :deck ["Komainu"]}})
      (play-from-hand state :corp "Komainu" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Nero ability")
      (is (= "Do you want to jack out?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (run-on state "HQ")
      (run-continue state)
      (is (not (= "Do you want to jack out?" (:msg (prompt-map :runner))))
          "No prompt for Nero ability because we used it on previous run")))
  (testing "Receives prompt on second run, if ability not used"
    (do-game
      (new-game {:runner {:id "Nero Severn: Information Broker"
                          :deck [(qty "Sure Gamble" 10)]}
                :corp { :deck ["Guard"]}})
      (play-from-hand state :corp "Guard" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Nero ability")
      (is (= "Do you want to jack out?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "No")
      (fire-subs state (get-ice state :hq 0))
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be again waiting on Runner for Nero ability")
      (is (= "Do you want to jack out?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes"))))

(deftest new-angeles-sol-your-news
  ;; New Angeles Sol - interaction with runner stealing agendas
  (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck [(qty "Paywall Implementation" 2) "Breaking News"]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Paywall Implementation")
    (take-credits state :corp)
    (is (= 6 (:credit (get-corp))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-corp))) "Corp gained 1cr from successful run")
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-corp))) "Paywall trashed before Sol triggers")
    (click-card state :corp (find-card "Paywall Implementation" (:hand (get-corp))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall back in play")))

(deftest next-design-guarding-the-net
  ;; Next Design.  Install up to 3 ICE before game starts, one per server max, and re-draw to 5
  (do-game
    (new-game {:corp {:id "NEXT Design: Guarding the Net"
                      :deck [(qty "Snowflake" 10)]}
               :options {:dont-start-turn true}})
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "HQ")
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "R&D")
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (= 5 (count (:hand (get-corp)))) "Corp should start with 5 cards in hand")))

(deftest nisei-division-the-next-generation
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game {:corp {:id "Nisei Division: The Next Generation"
                      :deck [(qty "Snowflake" 2)]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (let [s1 (get-ice state :hq 0)
          s2 (get-ice state :hq 1)]
      (run-on state "HQ")
      (core/rez state :corp s2)
      (run-continue state)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s2 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game")
      (run-continue state)
      (core/rez state :corp s1)
      (run-continue state)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s1 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game"))))

(deftest noise-hacker-extraordinaire
  ;; Noise: Hacker Extraordinaire
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)]}
               :runner {:id "Noise: Hacker Extraordinaire"
                        :deck ["Datasucker" "Cache" "Sure Gamble" (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]}})
    (starting-hand state :runner ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-corp)))) "Corp should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-corp)))) "Corp deck should contain 5 cards")
    (take-credits state :corp)
    (is (zero? (count (:discard (get-corp)))) "Archives started empty")
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
    (let [chip (get-hardware state 0)]
      (card-ability state :runner chip 0)
      (click-card state :runner (find-card "Cache" (:discard (get-runner))))
      (let [ds (get-program state 1)]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing virus via Clone Chip on corp's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
    (let [chip-2 (get-hardware state 0)]
      (card-ability state :runner chip-2 0)
      (click-card state :runner (find-card "Sharpshooter" (:discard (get-runner))))
      (let [ss (get-program state 2)]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing non-virus via Clone Chip on corp's turn should not trigger Noise ability")))

(deftest null-whistleblower
  ;; Null
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Wraparound" 3)]}
                 :runner {:id "Null: Whistleblower"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [wrap1 (get-ice state :hq 0)
            wrap2 (get-ice state :hq 1)]
        (is (empty? (:prompt (get-runner))) "Ability won't work on unrezzed ICE")
        (core/rez state :corp wrap2)
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
        (run-continue state)
        (core/rez state :corp wrap1)
        (run-continue state)
        (is (empty? (:prompt (get-runner))) "Ability already used this turn")
        (run-jack-out state)
        (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))
  (testing "does not affect next ice when current is trashed. Issue #1788."
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Spiderweb"]}
                 :runner {:id "Null: Whistleblower"
                          :deck [(qty "Parasite" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (let [spider (get-ice state :hq 1)
            iw (get-ice state :hq 0)]
        (core/rez state :corp spider)
        (core/rez state :corp iw)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner (refresh spider))
        (run-on state "HQ")
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner (first (:hand (get-runner))))
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Null")
        (is (= 1 (:current-strength (refresh iw))) "Ice Wall not reduced by Null"))))
  (testing "Receives prompt on second run, if ability not used"
    (do-game
      (new-game {:runner {:id "Null: Whistleblower"
                          :deck [(qty "Sure Gamble" 10)]}
                :corp { :deck ["Guard"]}})
      (play-from-hand state :corp "Guard" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Null ability")
      (is (= "Trash a card in grip to lower ice strength by 2?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "No")
      (fire-subs state (get-ice state :hq 0))
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be again waiting on Runner for Null ability")
      (is (= "Trash a card in grip to lower ice strength by 2?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes"))))

(deftest omar-keung-conspiracy-theorist
  ;; Omar Keung
  (testing "Make a successful run on the chosen server once per turn"
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])]
        (card-ability state :runner omar 0)
        (run-continue state)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (= "You accessed Hedge Fund." (:msg (prompt-map :runner))))
        (click-prompt state :runner "No action")
        (is (= 3 (:click (get-runner))))
        (card-ability state :runner omar 0)
        (is (= 3 (:click (get-runner))))
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :rd)
        (is (= [:rd] (get-in @state [:runner :register :successful-run])))
        (card-ability state :runner omar 0)
        (run-continue state)
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
        (run-continue state)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= (:cid ash) (-> (prompt-map :runner) :card :cid)))
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
        (run-continue state)
        (run-successful state)
        (is (= (:cid cr) (-> (prompt-map :runner) :card :cid)))
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
        (run-continue state)
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
        (run-continue state)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= 1 (get-counters (refresh nerve) :virus))))))
  (testing "Moving to a different server shouldn't trigger ability. Issue #3969"
    (do-game
      (new-game {:corp {:hand ["Bullfrog"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"}})
      (play-from-hand state :corp "Bullfrog" "Archives")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            frog (get-ice state :archives 0)]
        (card-ability state :runner omar 0)
        (core/rez state :corp frog)
        (run-continue state)
        (is (= [:archives] (:server (get-run))))
        (card-subroutine state :corp frog 0)
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "1 [Credits]")
        (click-prompt state :corp "R&D")
        (is (= [:rd] (:server (get-run))))
        (run-continue state)
        (run-successful state)
        (is (empty? (prompt-map :corp)))
        (is (empty? (prompt-map :runner)))))))

(deftest quetzal-free-spirit
  ;; Quetzal
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
               :runner {:id "Quetzal: Free Spirit"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [q (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)
          qmsg "break 1 Barrier subroutine"]
      (core/rez state :corp iwall)
      (run-continue state)
      (card-ability state :runner q 0)
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (refresh q) 0)
      (is (not (last-log-contains? state qmsg)) "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (refresh q) 0)
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (core/jack-out state :runner nil))))

(deftest reina-roja-freedom-fighter
  ;; Reina Roja - Increase cost of first rezzed ICE
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 3)]}
               :runner {:id "Reina Roja: Freedom Fighter"}})
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (core/rez state :corp quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))

(deftest rielle-kit-peddler-transhuman
  ;; Rielle "Kit" Peddler - Give ICE Code Gate
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
               :runner {:id "Rielle \"Kit\" Peddler: Transhuman"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [k (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)]
      (core/rez state :corp iwall)
      (run-continue state)
      (card-ability state :runner k 0)
      (is (has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))

(deftest saraswati-mnemonics-endless-exploration
  ;; Saraswati Mnemonics
  (do-game
    (new-game {:corp {:id "Saraswati Mnemonics: Endless Exploration"
                      :deck ["Gene Splicer" "House of Knives"]}})
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (click-card state :corp (find-card "Gene Splicer" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (let [splicer (get-content state :remote1 0)]
      (is (= 1 (get-counters (refresh splicer) :advancement)) "1 advancements placed on Gene Splicer")
      (core/rez state :corp (refresh splicer))
      (is (not (rezzed? (refresh splicer))) "Gene Splicer did not rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh splicer))
      (is (rezzed? (refresh splicer)) "Gene Splicer now rezzed")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "House of Knives" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [house (get-content state :remote2 0)]
        (advance state house)
        (advance state house)
        (core/score state :corp (refresh house))
        (is (empty? (:scored (get-corp))) "House of Knives not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/score state :corp (refresh house))
        (is (= 1 (:agenda-point (get-corp))) "House of Knives was able to be scored")))))

(deftest silhouette-stealth-operative
  ;; Silhouette
  (testing "Expose trigger ability resolves completely before access. Issue #2173"
    (do-game
      (new-game {:corp {:hand ["Psychic Field" "Fetal AI"]
                        :deck [(qty "Sure Gamble" 10)]}
                 :runner {:id "Silhouette: Stealth Operative"
                          :hand ["Feedback Filter" "Inside Job"]}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let [psychic (get-content state :remote1 0)
            ff (get-hardware state 0)]
        (run-empty-server state :hq)
        (is (:run @state) "On successful run trigger effects")
        (click-card state :runner psychic)
        (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
        (click-prompt state :corp "2 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (card-ability state :runner ff 0)
        (is (zero? (:credit (get-runner))) "Runner has no more credits left")
        (is (= 1 (count (:hand (get-runner)))) "Prevented 1 net damage")
        (is (empty? (:discard (get-runner))) "No cards discarded")
        (is (:run @state) "On run access phase")
        (click-prompt state :runner "Done")
        (is (empty? (:hand (get-runner))) "Suffered 1 net damage due to accessing Fetal AI")
        (is (= 1 (count (:discard (get-runner)))) "Discarded 1 card due to net damage")
        (is (:run @state) "Resolving access triggers")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "Runner has no credits to be able to steal Fetal AI")
        (is (not (:run @state)) "Run has now ended")
        (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
  (testing "with Temüjin; broken interaction with other successful-run triggers. Issue #1968"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign" (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)]}
                 :runner {:id "Silhouette: Stealth Operative"
                          :deck ["Temüjin Contract" "Desperado"]}})
      (starting-hand state :corp ["Hedge Fund" "PAD Campaign"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "HQ")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "Temüjin Contract")
      (click-card state :runner (get-content state :remote1 0))
      (click-prompt state :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
      (is (= 8 (:credit (get-runner))) "Gained 4cr")
      ;; second run
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 12 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin"))))

(deftest skorpios-defense-systems-persuasive-power
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game {:corp {:id "Skorpios Defense Systems: Persuasive Power"
                      :deck ["Hedge Fund" (qty "Quandary" 4)]}
               :runner {:deck ["The Maker's Eye" "Lucky Find"]}})
    (play-from-hand state :corp "Hedge Fund")
    (dotimes [_ 4] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (play-from-hand state :runner "Lucky Find")
    (play-from-hand state :runner "The Maker's Eye")
    (is (= [:rd] (:server (get-run))))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-corp) :prompt first :choices))) "No Maker's Eye choice")
    (click-prompt state :corp "Cancel")
    (run-continue state)
    (run-successful state)
    (is (= "You accessed Quandary." (:msg (prompt-map :runner))) "1st quandary")
    (click-prompt state :runner "No action")
    (is (= "You accessed Quandary." (:msg (prompt-map :runner))) "2nd quandary")
    (click-prompt state :runner "No action")
    (is (= "You accessed Quandary." (:msg (prompt-map :runner))) "3rd quandary")
    (click-prompt state :runner "No action")
    (is (not (:run @state)))
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (click-prompt state :corp (find-card "The Maker's Eye" (:discard (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rfg]))) "One card RFGed")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (:prompt (get-corp))) "Cannot use Skorpios twice")))

(deftest spark-agency-worldswide-reach
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game {:corp {:id "Spark Agency: Worldswide Reach"
                      :deck [(qty "Launch Campaign" 3)]}})
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

(deftest sportsmetal-go-big-or-go-home
  ;; SportsMetal - gain 2 credits or draw 2 cards on agenda scored or stolen
  (testing "Gain 2 credits on score"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :corp "Gain 2 credits")
      (is (= 7 (:credit (get-corp))) "Corp gains 2 credits")))
  (testing "Gain 2 credits on steal"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Gain 2 credits")
      (is (= 9 (:credit (get-corp))) "Corp gains 2 credits")))
  (testing "Draw 2 cards on score"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger" (qty "Hedge Fund" 2)]}})
      (starting-hand state :corp ["Merger"])
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (empty? (:hand (get-corp))) "Corp starts with no cards")
      (click-prompt state :corp "Draw 2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards")))
  (testing "Draw 2 cards on steal"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger" (qty "Hedge Fund" 2)]}})
      (starting-hand state :corp ["Merger"])
      (play-from-hand state :corp "Merger" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (empty? (:hand (get-corp))) "Corp starts with no cards")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Draw 2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards"))))

(deftest sso-industries-fueling-innovation
  ;; SSO Industries: Fueling Innovation - add advancement tokens on ice for faceup agendas
  (do-game
    (new-game {:corp {:id "SSO Industries: Fueling Innovation"
                      :deck [(qty "Hortum" 2) (qty "Oaktown Renovation" 2) "Braintrust"]}})
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
      (is (zero? (get-counters (refresh h0) :advancement)) "Starts with 0 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Starts with 0 tokens")
      (take-credits state :corp)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh h0))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Gains 2 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Stays at 0 tokens")
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (take-credits state :corp)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh h1))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Stays at 2 tokens")
      (is (= 4 (get-counters (refresh h1) :advancement)) "Gains 4 tokens")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (empty? (:prompt (get-corp))) "Not prompted when all ice advanced"))))

(deftest steve-cambridge-master-grifter
  ;; Steve
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hedge Fund"]}
               :runner {:id "Steve Cambridge: Master Grifter"
                        :discard ["Sure Gamble" "Easy Mark"]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-card state :runner "Sure Gamble")
    (click-card state :runner "Easy Mark")
    (click-prompt state :corp "Sure Gamble")
    (click-prompt state :runner "No action")
    (is (= "Easy Mark" (-> (get-runner) :hand first :title)) "Easy Mark should be in the hand")
    (is (= "Sure Gamble" (-> (get-runner) :rfg first :title)) "Sure Gamble should be removed from game")))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward
  (do-game
    (new-game {:corp {:id "Strategic Innovations: Future Forward"
                      :deck [(qty "Hedge Fund" 2) (qty "Eli 1.0" 2) (qty "Crick" 2)]}})
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
      (click-card state :corp (first (:discard (get-corp))))
      (is (empty? (:discard (get-corp))) "Hedge Fund moved back to R&D")
      (take-credits state :corp)
      (core/rez state :corp i2)
      (take-credits state :runner)
      (is (zero? (count (:prompt (get-corp))))
          "Corp not prompted to trigger Strategic Innovations"))))

(deftest sync-everything-everywhere
  ;; SYNC: Everything, Everywhere
  (do-game
    (new-game {:corp {:id "SYNC: Everything, Everywhere"}
               :runner {:deck ["Fan Site"]}
               :options {:start-as :runner}})
    (play-from-hand state :runner "Fan Site")
    (gain-tags state :runner 1)
    (is (= 1 (count-tags state)) "Runner has 1 tag")
    (changes-val-macro -3 (:credit (get-runner))
                       "Paid 3c to remove tag"
                       (core/remove-tag state :runner nil))
    (is (= 0 (count-tags state)) "Runner removed tag")
    (take-credits state :runner)
    (gain-tags state :runner 1)
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (swap! state assoc-in [:corp :credit] 0)
    (changes-val-macro 0 (:credit (get-runner))
                       "Paid 0c to trash resource"
                       (core/trash-resource state :corp nil)
                       (click-card state :corp (get-resource state 0)))
    (is (= ["Fan Site"] (map :title (:discard (get-runner)))) "Trashed Fan Site")))

(deftest the-foundry-refining-the-process
  ;; The Foundry
  (testing "interaction with Accelerated Beta Test"
    (do-game
      (new-game {:corp {:id "The Foundry: Refining the Process"
                        :deck [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)]}})
      (starting-hand state :corp ["Accelerated Beta Test"])
      (play-from-hand state :corp "Accelerated Beta Test" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "Eli 1.0")
      (click-prompt state :corp "Archives")
      (click-prompt state :corp "Yes")
      (is (empty? (:play-area (get-corp))) "Play area shuffled into R&D")
      (is (= 1 (count (:hand (get-corp)))) "Added Eli 1.0 to HQ"))))

(deftest the-outfit-family-owned-and-operated
  ;; The Outfit - Gain 3 whenever you take at least 1 bad publicity
  (testing "basic test"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Hostile Takeover" "Profiteering"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 1 (count-bad-pub state)) "Take 1 bad publicity")
      (is (= 15 (:credit (get-corp))) "Corp should gain 10 credits")
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (click-prompt state :corp "3")  ;; Take 3 bad publicity from Profiteering, gain 15
      (is (= 4 (count-bad-pub state)) "Corp should gain 1 bad publicity")
      (is (= 33 (:credit (get-corp))) "Corp should gain 18 credits")))
  (testing "with Profiteering - Only gain 3 credits when taking more than 1 bad publicity in a single effect"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Profiteering"]}})
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "3")
      (is (= 3 (count-bad-pub state)) "Take 3 bad publicity")
      (is (= 23 (:credit (get-corp))) "Gain 15 from Profiteering + 3 from The Outfit")))
  (testing "vs Valencia - 1 bad pub at start means 5 credits to start with (does not _gain_ BP)"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Hostile Takeover"]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 1 (count-bad-pub state)) "The Outfit starts with 1 bad publicity")
      (is (= 5 (:credit (get-corp))) "The Outfit starts with 8 credits")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 2 (count-bad-pub state)) "Take 1 bad publicity")
      (is (= (+ 5 7 3) (:credit (get-corp))) "Gain 7 from Hostile Takeover + 3 from The Outfit"))))

(deftest titan-transnational-investing-in-your-future
  ;; Titan Transnational
  (testing "Add a counter to a scored agenda"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atl (get-content state :remote1 0)]
        (core/gain state :corp :click 1)
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/score state :corp {:card (refresh atl)})
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))
  (testing "only use one counter of Corporate Sales Team"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Corporate Sales Team" "Mark Yale"]}})
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
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
          (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
          (core/rez state :corp my)
          (card-ability state :corp my 1)
          (click-card state :corp (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
          (card-ability state :corp my 1)
          (is (empty? (:prompt (get-corp))) "No prompt for the Corp as no counters exist to spend")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale"))))))

(deftest weyland-consortium-because-we-built-it
  ;; Weyland Consortium: Because We Built It
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Because We Built It"
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (let [iw (get-ice state :remote1 0)
            bwbi (get-in @state [:corp :identity])]
        (changes-val-macro 0 (:credit (get-corp))
                           "Used 1 credit from Weyland BWBI to advance Ice Wall"
                           (core/advance state :corp {:card (refresh iw)})
                           (click-card state :corp bwbi))))))

(deftest weyland-consortium-builder-of-nations
  ;; Builder of Nations
  (testing "1 meat damage per turn at most"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (advance state (get-ice state :hq 0) 1)
      (take-credits state :corp)
      (run-on state "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 meat damage from BoN")
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (run-continue state)
      (is (= 1 (count (:discard (get-runner)))) "Runner took only 1 meat damage from BoN total")
      (is (zero? (count (:prompt (get-corp)))))))
  (testing "2 meat damage from ID ability when The Cleaners is scored"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "The Cleaners" 3) (qty "Ice Wall" 3)]}
                 :runner {:deck [(qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (advance state (get-ice state :hq 0) 1)
      (play-from-hand state :corp "The Cleaners" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from BoN/Cleaners combo")))
  (testing "trashing a solo ice on an empty server still triggers #5025"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Corroder" "Hippo" "Sure Gamble"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (advance state (get-ice state :remote1 0) 1)
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Hippo")
      (run-on state :remote1)
      (core/rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (is (nil? (get-ice state :remote1 0)) "Ice Wall is trashed")
      (is (nil? (:run @state)) "Ice Wall is trashed, so run has been ended")
      (is (= 1 (count (:discard (get-runner))))))))

(deftest whizzard-master-gamer
  ;; Whizzard
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["PAD Campaign" "Shell Corporation"]}
               :runner {:id "Whizzard: Master Gamer"
                        :credits 1}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (let [pad (get-content state :remote1 0)
          whiz (:identity (get-runner))]
      (run-empty-server state :remote1)
      (is (= 1 (:credit (get-runner))) "Runner can't afford to trash PAD Campaign")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (dotimes [_ 3]
        (click-card state :runner (refresh whiz)))
      (is (nil? (refresh pad)) "PAD Campaign successfully trashed"))))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced
  (do-game
    (new-game {:corp {:deck [(qty "Launch Campaign" 3)]}
               :runner {:id "Wyvern: Chemically Enhanced"
                        :deck [(qty "Sure Gamble" 2) "Corroder"
                               "Clone Chip" "Easy Mark"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Easy Mark")
    (play-from-hand state :runner "Corroder")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 2 [Credits] to trash")  ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-runner))) "Easy Mark moved to deck")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Pay 2 [Credits] to trash")
    (is (= "Sure Gamble" (:title (last (:discard (get-runner))))) "Sure Gamble still in Wyvern's discard")))
