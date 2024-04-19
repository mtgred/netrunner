(ns game.cards.identities-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.mark :refer [is-mark?]]
   [game.core.servers :refer [unknown->kw zone->name]]
   [game.test-framework :refer :all]
   [game.utils :as utils]))

(deftest ^{:card-title "419-amoral-scammer"}
  FourHundredAndNineTeen-amoral-scammer
  ;; 419
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
      (is (no-prompt? state :runner) "No option on second install")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Archives")
      (click-prompt state :runner "No")
      (is (no-prompt? state :corp) "No prompt if Runner chooses No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "The Cleaners" "New remote")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (is (last-log-contains? state "exposes The Cleaners") "Installed card was exposed")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (no-prompt? state :corp) "Cannot expose faceup agendas")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/lose state :corp :credit (:credit (get-corp)))
      (is (zero? (:credit (get-corp))) "Corp has no credits")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (click-prompt state :runner "Yes")
      (is (no-prompt? state :corp) "No prompt if Corp has no credits")
      (is (last-log-contains? state "exposes PAD Campaign") "Installed card was exposed")))

(deftest FourHundredAndNineTeen-amoral-scammer-verify-expose-can-be-blocked
    ;; Verify expose can be blocked
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
        (rez state :corp ug)
        (click-prompt state :corp "No")
        (is (last-log-contains? state "uses Underway Grid to prevent 1 card from being exposed") "Exposure was prevented"))))

(deftest FourHundredAndNineTeen-amoral-scammer-ixodidae-shouldn-t-trigger-off-419-s-ability
    ;; Ixodidae shouldn't trigger off 419's ability
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

(deftest FourHundredAndNineTeen-amoral-scammer-419-vs-asa-group-double-install-corp-s-turn
    ;; 419 vs Asa Group double install, Corp's turn
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["PAD Campaign" "Ice Wall"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (click-card state :corp "Ice Wall")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (is (last-log-contains? state "exposes PAD Campaign in Server 1") "Installed card was exposed")))

(deftest FourHundredAndNineTeen-amoral-scammer-419-vs-asa-group-double-install-runner-s-turn
    ;; 419 vs Asa Group double install, Runner's turn
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["PAD Campaign" "Ice Wall" "Advanced Assembly Lines"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Advanced Assembly Lines" "New remote")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "No")
      (take-credits state :corp)
      (rez state :corp (get-content state :remote1 0))
      (card-ability state :corp (get-content state :remote1 0) 0)
      (click-card state :corp "PAD Campaign")
      (click-prompt state :corp "New remote")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (is (last-log-contains? state "exposes PAD Campaign in Server 2") "Installed card was exposed")
      (is (prompt-is-type? state :corp :select) "Corp should still have select prompt")))

(deftest FourHundredAndNineTeen-amoral-scammer-interation-with-install-and-rez-effects-issue-4485
    ;; interation with 'install and rez' effects. Issue #4485
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Building Blocks" "Ice Wall"]
                        :credits 10}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Building Blocks")
      (click-card state :corp "Ice Wall")
      (click-prompt state :corp "HQ")
      (is (no-prompt? state :runner) "419 doesn't trigger on installed and rezzed cards")))

(deftest FourHundredAndNineTeen-amoral-scammer-419-vs-sportsmetal-jinja-grid-issue-3806
    ;; 419 vs Sportsmetal Jinja Grid. Issue #3806
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck [(qty "Ice Wall" 5)]
                        :hand ["Domestic Sleepers" "Jinja City Grid"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Jinja City Grid" "New remote")
      (rez state :corp (get-content state :remote1 0))
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
      (is (prompt-is-type? state :runner :waiting) "Runner should wait again")))

(deftest a-teia-ip-recovery
  (do-game
    (new-game {:corp {:id "A Teia: IP Recovery"
                      :hand ["Ice Wall" "Vanilla" "PAD Campaign" "Spin Doctor" "Enigma" "Pharos"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (no-prompt? state :corp) "No trigger when installing on central servers")
    (play-from-hand state :corp "Spin Doctor" "New remote")
    (click-card state :corp "Vanilla")
    (click-prompt state :corp "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (is (= 2 (count (core/get-remotes state))) "Could not install more remotes")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Enigma" "Server 1")
    (is (changed? [(:credit (get-corp)) 0]
          (click-card state :corp "Pharos")
          (click-prompt state :corp "Server 2"))
        "Ignored install costs")
    ))

(deftest a-teia-tatu-bola
  (do-game
    (new-game {:corp {:id "A Teia: IP Recovery"
                      :hand ["Tatu-Bola" "Vanilla"]}})
    (play-from-hand state :corp "Tatu-Bola" "New remote")
    (click-prompt state :corp "Done")
    (take-credits state :corp)
    (run-on state :remote1)
    (rez state :corp (get-ice state :remote1 0))
    (run-continue state :encounter-ice)
    (run-continue state :pass-ice)
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Vanilla")
    (click-card state :corp "Tatu-Bola")
    (click-prompt state :corp "New remote")))

(deftest acme-consulting-the-truth-you-need-tag-gain-when-rezzing-outermost-ice
    ;; Tag gain when rezzing outermost ice
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (is-tagged? state)) "Runner does not encounter an unrezzed ice")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner no longer encountering outermost ice")))

(deftest acme-consulting-the-truth-you-need-interaction-with-data-ward
    ;; Interaction with Data Ward
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Data Ward"]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Data Ward" "Archives")
      (take-credits state :corp)
      (run-on state "Archives")
      (is (not (is-tagged? state)) "Runner does not encounter an unrezzed ice")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Take 1 tag")
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (card-subroutine state :corp (get-ice state :archives 0) 0)
      (is (not (:run @state)) "Run ended by Data Ward")))

(deftest acme-consulting-the-truth-you-need-tag-gain-when-starting-run
    ;; Tag gain when starting run
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner no longer encountering outermost ice")))

(deftest acme-consulting-the-truth-you-need-tag-loss-when-derezzing-ice
    ;; Tag loss when derezzing ice
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (derez state :corp (get-ice state :archives 0))
      (is (not (is-tagged? state)) "Runner no longer encountering the derezzed ice")))

(deftest acme-consulting-the-truth-you-need-no-tag-on-empty-server
    ;; No tag on empty server
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                      :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (take-credits state :corp)
      (click-card state :corp (first (:hand (get-corp))))
      (run-on state :archives)
      (is (not (is-tagged? state)) "No ice to encounter")))

(deftest acme-consulting-the-truth-you-need-no-tag-when-encountering-second-ice
    ;; No tag when encountering second ice
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Vanilla" 2) (qty "Hedge Fund" 4)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "Archives")
      (rez state :corp (get-ice state :archives 0))
      (rez state :corp (get-ice state :archives 1))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner is not tagged when encountering second ice")))

(deftest acme-consulting-the-truth-you-need-tag-loss-when-runner-jacks-out
    ;; Tag loss when runner jacks out
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (is-tagged? state) "Runner is tagged when encountering outermost ice")
      (run-continue state :movement)
      (run-jack-out state)
      (is (not (is-tagged? state)) "Runner no longer tagged after jacking out")))

(deftest acme-consulting-the-truth-you-need-no-tag-gained-when-rezzing-something-other-than-ice
    ;; No tag gained when rezzing something other than ice
    (do-game
      (new-game
        {:corp {:id "Acme Consulting: The Truth You Need"
                :deck ["Vanilla" "NGO Front"]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "NGO Front" "New remote")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (is-tagged? state)) "Runner is not yet tagged when encountering outermost ice")
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (= 1 (core/sum-effects state :runner :tags)) "Runner gains 1 additional tag when ice rezzed")
      (rez state :corp (get-content state :remote1 0))
      (is (rezzed? (get-content state :remote1 0)) "NGO Front now rezzed")
      (is (= 1 (core/sum-effects state :runner :tags)) "Runner does not gain a tag when asset rezzed")
      (run-continue state)
      (is (not (is-tagged? state)) "Runner is not tagged when encountering second ice")))

(deftest acme-consulting-the-truth-you-need-trashing-the-ice-removes-the-tag-4984
    ;; Trashing the ice removes the tag #4984
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Ice Wall"]}
                 :runner {:deck ["Corroder" "Hippo"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Corroder")
      (run-on state "Server 1")
      (rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (is (zero? (count-tags state)) "Acme additional tag falls off")))

(deftest acme-consulting-the-truth-you-need-interactions-with-fly-on-the-wall-5227
    ;; Interactions with Fly on the Wall #5227
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Fly on the Wall" "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-and-score state "Fly on the Wall")
      (is (is-tagged? state) "Runner should be tagged")
      (is (= 1 (count-tags state)) "Fly on the Wall gives 1 tag")
      (take-credits state :corp)
      (run-on state "Server 1")
      (rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (is (is-tagged? state) "Runner should be tagged")
      (is (= 2 (count-tags state)) "Acme gives real tags")
      (fire-subs state (get-ice state :remote1 0))
      (is (is-tagged? state) "Runner should be tagged")
      (is (= 1 (count-tags state)) "Acme's tag falls off")))

(deftest acme-consulting-the-truth-you-need-interactions-with-data-ward-5178
    ;; Interactions with Data Ward #5178
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Data Ward"]
                        :credits 10}
                 :runner {:hand ["Corroder" "Xanadu"]
                          :credits 15}})
      (play-from-hand state :corp "Data Ward" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Xanadu")
      (play-from-hand state :runner "Corroder")
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (is (is-tagged? state) "Runner should be tagged")
      (is (= 1 (count-tags state)) "Acme gives real tags")
      (click-prompt state :runner "Take 1 tag")
      (is (= 2 (count-tags state)))
      (auto-pump-and-break state (get-program state 0))
      (core/continue state :corp nil)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (is-tagged? state) "Runner should still be tagged")
      (is (= 1 (count-tags state)) "Acme's tag falls off")
      (take-credits state :runner)
      (trash-resource state)
      (click-card state :corp (get-resource state 0))
      (is (= "Xanadu" (:title (get-discarded state :runner 0))))))

(deftest acme-consulting-the-truth-you-need-tagged-when-encountering-outermost-ice-on-a-different-server-than-the-attacked-server
    ;; Tagged when encountering outermost ice on a different server than the attacked server
    (do-game
     (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                       :deck [(qty "Hedge Fund" 5)]
                       :hand ["Konjin" "Ice Wall" "Enigma"]
                       :credits 10}})
     (play-from-hand state :corp "Konjin" "HQ")
     (play-from-hand state :corp "Enigma" "HQ")
     (play-from-hand state :corp "Ice Wall" "R&D")
     (take-credits state :corp)
     (let [konjin (get-ice state :hq 0)
           iw (get-ice state :rd 0)]
       (rez state :corp konjin)
       (rez state :corp iw)
       (run-on state "HQ")
       (run-continue-until state :encounter-ice konjin)
       (is (not (is-tagged? state)) "Runner is not tagged while encountering Konjin")
       (click-prompt state :corp "0 [Credits]")
       (click-prompt state :runner "1 [Credits]")
       (click-card state :corp iw)
       (is (is-tagged? state) "Runner should be tagged while encountering Ice Wall")
       (encounter-continue state)
       (is (= :encounter-ice (:phase (get-run))) "Still encountering Konjin")
       (is (not (is-tagged? state)) "Runner is no longer tagged"))))

(deftest adam-compulsive-hacker-allow-runner-to-choose-directives
    ;; Allow runner to choose directives
    (do-game
      (new-game {:runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}
                 :options {:dont-start-game true}})
      (is (= 4 (count (get-in @state [:runner :play-area]))) "All directives are in the runner's play area")
      (is (zero? (count (get-in @state [:runner :hand]))))
      (click-card state :runner (find-card "Neutralize All Threats" (:play-area (get-runner))))
      (click-card state :runner (find-card "Safety First" (:play-area (get-runner))))
      (click-card state :runner (find-card "Always Be Running" (:play-area (get-runner))))
      (is (= 3 (count (get-resource state))) "3 directives were installed")
      (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
      (let [nat (find-card "Neutralize All Threats" (get-resource state))
            sf (find-card "Safety First" (get-resource state))
            abr (find-card "Always Be Running" (get-resource state))]
        (is (and nat sf abr) "The chosen directives were installed"))))

(deftest adam-compulsive-hacker-directives-should-not-grant-palana-credits
    ;; Directives should not grant Pālanā credits
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
      (start-turn state :corp)
      (is (= 5 (:credit (get-corp))) "Pālanā does not gain credit from Adam's starting Directives")))

(deftest adam-compulsive-hacker-neutralize-all-threats-interaction-with-advanceable-traps
    ;; Neutralize All Threats interaction with advanceable traps
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
      (start-turn state :corp)
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (advance state (get-content state :remote1 0) 2)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (= 2 (:brain-damage (get-runner))) "Runner took 2 core damage")
      (is (= 1 (count (:discard (get-corp)))) "1 card in archives")))

(deftest aginfusion-new-miracles-for-a-new-world-ability-works-5056
    ;; Ability works. #5056
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
        (is (utils/same-card? (get-ice state :hq 0) (:ice (ffirst ice-passed-last-run)))))
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (nil? (get-run)))
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))))

(deftest aginfusion-new-miracles-for-a-new-world-works-with-passing-effects
    ;; Works with passing effects
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
      (run-continue state)
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "En Passant")
      (click-card state :runner "Kakugo")
      (is (nil? (get-ice  state :hq 0)))
      (is (find-card "Kakugo" (:discard (get-corp))))))

(deftest aginfusion-new-miracles-for-a-new-world-uses-up-on-encounter-bypass-effects
    ;; Uses up on-encounter bypass effects
    (do-game
      (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund" "Ice Wall" "Vanilla" "Kakugo"]
                        :credits 10}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Kakugo" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "R&D")
      (is (= (get-ice state :rd 0) (core/get-current-ice state)))
      (card-ability state :corp (:identity (get-corp)) 0)
      (click-prompt state :corp "HQ")
      (is (= (get-ice state :hq 1) (core/get-current-ice state)) "At the outermost ice of HQ")
      (is (= :movement (:phase (:run @state))) "Encounter has ended and are in the movement phase")
      (run-continue state :approach-ice)
      (run-continue state)
      (is (last-log-contains? state "Runner encounters Kakugo protecting HQ at position 0."))))

(deftest aginfusion-new-miracles-for-a-new-world-moving-runner-to-un-iced-server-lets-them-jack-out
    ;; moving runner to un-iced server lets them jack out
    (do-game
      (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund" "Ice Wall" "Kakugo"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (run-on state "R&D")
      (is (= (get-ice state :rd 0) (core/get-current-ice state)))
      (card-ability state :corp (:identity (get-corp)) 0)
      (click-prompt state :corp "HQ")
      (run-jack-out state)))

(deftest akiko-nisei-head-case
  ;; Akiko Nisei
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand [(qty "Hedge Fund" 4)]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 2 (:random-access-limit (core/num-cards-to-access state :runner :rd nil))) "Should access additional card from ability")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 1 (:random-access-limit (core/num-cards-to-access state :runner :rd nil))) "Should only access 1 from missed psi game")))

(deftest akiko-nisei-head-case-shiro-interaction-second-sub-should-give-akiko-2-accesses
    ;; Shiro interaction: second sub should give Akiko 2 accesses
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Shiro"]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Shiro" "New remote")
      (take-credits state :corp)
      (let [shiro (get-ice state :remote1 0)]
        (rez state :corp shiro)
        (run-on state :remote1)
        (run-continue state)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "No")
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 2 (:random-access-limit (core/num-cards-to-access state :runner :rd nil))) "Should access additional card from ability")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (run-continue state)
        (run-continue state)
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :remote1)
        (run-continue state)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "No")
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 1 (:random-access-limit (core/num-cards-to-access state :runner :rd nil))) "Should only access 1 from missed psi game"))))

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
  (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 1 (get-link state)) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-dispossessed-ristie-9-card-starting-hand-after-mulligan
    ;; 9 card starting hand after mulligan
    (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}
                 :options {:mulligan :runner}})
      (is (= 1 (get-link state)) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-dispossessed-ristie-should-not-grant-palana-credits
    ;; should not grant Palana credits
    (do-game
      (new-game {:corp {:id "Pālanā Foods: Sustainable Growth"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 5 (:credit (get-corp))) "Palana does not gain credit from Andromeda's starting hand")))

(deftest apex-invasive-predator-allow-facedown-install-of-a-second-console-issue-1326
    ;; Allow facedown install of a second console. Issue #1326
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck [(qty "Heartbeat" 2)]}})
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-prompt state :runner "Done")
      (play-from-hand state :runner "Heartbeat")
      (is (= 1 (count (get-hardware state))))
      (take-credits state :runner)
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
      (is (= 1 (count (get-runner-facedown state))) "2nd console installed facedown")))

(deftest apex-invasive-predator-allow-facedown-install-of-program-when-over-mu
    ;; Allow facedown install of a program when MU is full
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck [(qty "Endless Hunger" 2)]}})
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-prompt state :runner "Done")
      (play-from-hand state :runner "Endless Hunger")
      (take-credits state :runner)
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-card state :runner (find-card "Endless Hunger" (:hand (get-runner))))
      (is (= 1 (count (get-runner-facedown state))) "Endless Hunger installed facedown")))

(deftest apex-invasive-predator-don-t-fire-events-when-installed-facedown-issue-4085
    ;; Don't fire events when installed facedown. Issue #4085
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Sure Gamble"]}})
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (let [credits (:credit (get-runner))]
        (click-card state :runner "Sure Gamble")
        (is (= credits (:credit (get-runner)))))))

(deftest apex-invasive-predator-spec-work-cannot-be-installed-facedown-4574
    ;; Spec Work cannot be installed facedown #4574
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Spec Work" "Sure Gamble" "Cache"]}})
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-card state :runner "Spec Work")
      (is (= 1 (count (get-runner-facedown state))) "Spec Work installed facedown")))

(deftest arissana-rocha-nahu-street-artist
  (do-game
    (new-game {:runner {:id "Arissana Rocha Nahu: Street Artist"
                        :hand ["Hush" "Cache"]}
               :corp {:hand ["Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (card-ability state :runner (:identity (get-runner)) 0)
    (is (no-prompt? state :runner) "Can't use Arissana ability outside of a run")
    (run-on state "Archives")
    (card-ability state :runner (:identity (get-runner)) 0)
    (is (changed? [(:credit (get-runner)) -1]
                  (click-prompt state :runner "Cache"))
        "No additional costs paid")
    (is (= "Cache" (:title (get-program state 0))))
    (run-continue state)
    (is (= 1 (count (:discard (get-runner)))) "Cache was trashed at the end of the run")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-on state "Archives")
    (card-ability state :runner (:identity (get-runner)) 0)
    (click-prompt state :runner "Hush")
    (click-card state :runner (get-ice state :hq 0))
    (is (= "Hush" (:title (first (:hosted (refresh (get-ice state :hq 0)))))))
    (run-continue state)
    (is (= 1 (count (:discard (get-runner)))) "Hush is still installed")))

(deftest armand-geist-walker-tech-lord-async-costs-with-sync-abilities
    ;; async costs with sync abilities
    (do-game
      (new-game {:runner {:id "Armand \"Geist\" Walker: Tech Lord"
                          :deck ["Sure Gamble" "Magnum Opus"]
                          :hand ["The Class Act" "All-nighter"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Class Act")
      (play-from-hand state :runner "All-nighter")
      (is (changed? [(:click (get-runner)) -1]
            (card-ability state :runner (get-resource state 1) "Gain [Click][Click]"))
          "While resolving the cost, runner doesn't gain any clicks")
      (is (changed? [(:click (get-runner)) 2]
            (click-card state :runner "Magnum Opus"))
          "After resolving the cost, runner gains 2 clicks")
      (is (last-n-log-contains? state 2 "Runner uses Armand \"Geist\" Walker: Tech Lord")
          "Geist prints first")
      (is (second-last-log-contains? state "Runner uses The Class Act")
        "The Class Act prints second, with no All-nighter yet")
      (is (last-log-contains? state "trashes All-nighter to use All-nighter")
          "All-nighter is now logged correctly, having paid all costs")))

(deftest asa-group-security-through-vigilance-asa-group-should-not-allow-installing-operations
    ;; Asa Group should not allow installing operations
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "BOOM!" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "BOOM!" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))

(deftest asa-group-security-through-vigilance-asa-group-should-not-allow-installing-agendas
    ;; Asa Group should not allow installing agendas
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "Project Vitruvius" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "Project Vitruvius" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group did not install Agenda with its ability")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))

(deftest asa-group-security-through-vigilance-asa-group-should-not-allow-installing-assets-on-central-servers
    ;; Asa Group should not allow installing assets on central servers
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "PAD Campaign" "Ben Musashi"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (click-card state :corp (find-card "PAD Campaign" (:hand (get-corp))))
      (is (empty? (get-content state :hq)) "Asa Group did not install asset on central server with its ability")
      (click-card state :corp (find-card "Ben Musashi" (:hand (get-corp))))
      (is (= "Ben Musashi" (:title (get-content state :hq 0))) "Asa Group can install an upgrade in a central server")))

(deftest asa-group-security-through-vigilance-asa-group-ordering-correct-when-playing-mirrormorph
    ;; Asa Group ordering correct when playing Mirrormorph
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
        (is (= "Choose a non-agenda card in HQ to install" (:msg (prompt-map :corp))))
        (click-card state :corp herrings)
        (is (= "Red Herrings" (:title (get-content state :remote1 1))) "Red Herrings is installed in Server 1")
        (click-card state :corp vitruvius)
        (click-prompt state :corp "New remote")
        (click-card state :corp pup)
        (click-prompt state :corp "New remote")
        (is (no-prompt? state :corp) "No more prompts")
        (is (= 6 (count (:servers (get-corp)))) "There are six servers, including centrals"))))

(deftest ayla-bios-rahim-simulant-specialist
  ;; Ayla - choose & use hosted cards
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
    (is (= 4 (count (:hosted (:identity (get-runner))))) "4 hosted cards")
    (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
    (click-prompt state :corp "Keep")
    (click-prompt state :runner "Keep")
    (take-credits state :corp)
    (is (= 2 (count (get-in @state [:runner :hand]))) "There are 2 cards in the grip")
    (card-ability state :runner (:identity (get-runner)) 0)
    (click-prompt state :runner (find-card "Bank Job" (:hosted (:identity (get-runner)))))
    (is (= 3 (count (get-in @state [:runner :hand]))) "There are 3 cards in the grip")))

(deftest az-mccaffrey-mechanical-prodigy
  ;; Az McCaffrey: Mechanical Prodigy
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

(deftest az-mccaffrey-mechanical-prodigy-test-for-interaction-with-hostage
    ;; Test for interaction with Hostage
    (do-game
      (new-game {:runner {:id "Az McCaffrey: Mechanical Prodigy"
                          :deck ["Hostage" "Professional Contacts"]
                          :hand ["Hostage"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hostage")
      (click-prompt state :runner "Professional Contacts")
      (click-prompt state :runner "Yes")
      (is (= "Professional Contacts" (:title (get-resource state 0))) "ProCo was correctly installed")
      (is (= (+ 5 -1 -5 1) (:credit (get-runner))) "Spent all credits. Was at 5, -1 hostage, -5 ProCo, +1 ID")))

(deftest azmari-edtech-shaping-the-future-don-t-gain-credits-when-installing-facedown-4477
    ;; Don't gain credits when installing facedown #4477
    (do-game
      (new-game {:corp {:id "Azmari EdTech: Shaping the Future"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:id "Apex: Invasive Predator"
                          :hand ["Sure Gamble"]}})
      (take-credits state :corp)
      (click-prompt state :corp "Event")
      (end-phase-12 state :runner)
      (let [credits (:credit (get-corp))]
        (click-card state :runner "Sure Gamble")
        (is (= credits (:credit (get-corp))) "Corp gains no credits from facedown install"))) )

(deftest blue-sun-powering-the-future
  ;; Blue Sun - Pick up cards at start of turn
  (do-game
    (new-game {:corp {:id "Blue Sun: Powering the Future"
                      :deck [(qty "Hedge Fund" 5)]
                      :hand ["Reduced Service"]}})
    (play-from-hand state :corp "Reduced Service" "New remote")
    (let [rs (get-content state :remote1 0)]
      (rez state :corp rs)
      (click-prompt state :corp "3")
      (is (= 3 (get-counters (refresh rs) :power)) "Reduced Service should have 3 counters on itself")
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp rs)
      (is (nil? (refresh rs)) "Reduced Service is picked up")
      (is (find-card "Reduced Service" (:hand (get-corp))) "Reduced Service is now in HQ"))
    (play-from-hand state :corp "Reduced Service" "New remote")
    (is (zero? (get-counters (get-content state :remote2 0) :power)) "Reduced Service should have 0 counters on itself after reinstall")))

(deftest captain-padma-isbister-intrepid-explorer
  (do-game
    (new-game {:runner {:id "Captain Padma Isbister: Intrepid Explorer"
                        :hand ["Earthrise Hotel"]}})
    (take-credits state :corp)
    (run-on state :rd)
    (is (no-prompt? state :runner) "no prompt to charge (no targets)")
    (run-jack-out state)
    (play-from-hand state :runner "Earthrise Hotel")
    (run-on state :rd)
    (is (no-prompt? state :runner) "no prompt to charge (chance already missed)")
    (run-jack-out state)
    (take-credits state :runner)
    (take-credits state :corp)
    (is (changed? [(get-counters (get-resource state 0) :power) +1]
          (run-on state :rd)
          (is (not (no-prompt? state :runner)) "prompt to charge")
          (click-card state :runner "Earthrise Hotel"))
        "Gained 1 power counter from charging hotel")
    (run-jack-out state)))

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
  (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :hand [(qty "Neural EMP" 2)]}
                 :runner {:deck [(qty "Imp" 3)]}})
      (take-credits state :corp)
      (damage state :corp :net 1)
      (click-prompt state :corp "Yes")
      (let [imp (find-card "Imp" (:hand (get-runner)))]
        (click-prompt state :corp imp)
        (is (= 1 (count (:discard (get-runner)))))
        (damage state :corp :net 1)
        (is (no-prompt? state :corp) "No choice on second net damage")
        (is (= 2 (count (:discard (get-runner)))))
        (run-empty-server state "Archives")
        (take-credits state :runner)
        (core/move state :runner (find-card "Imp" (:discard (get-runner))) :hand)
        (play-from-hand state :corp "Neural EMP")
        (click-prompt state :corp "No")
        (is (= 2 (count (:discard (get-runner)))) "Damage dealt after declining ability")
        (play-from-hand state :corp "Neural EMP")
        (is (no-prompt? state :corp) "No choice after declining on first damage")
        (is (= 3 (count (:discard (get-runner))))))))

(deftest chronos-protocol-selective-mind-mapping-with-obokata-pay-4-net-damage-to-steal-only-3-damage-left-after-chronos-no-trigger-of-damage-prevent
    ;; with Obokata: Pay 4 net damage to steal. Only 3 damage left after Chronos. No trigger of damage prevent.
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
      (is (no-prompt? state :runner) "Feedback Filter net damage prevention opportunity not given")
      (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 net damage")))

(deftest chronos-protocol-selective-mind-mapping-vs-employee-strike-issue-1958
    ;; vs Employee Strike. Issue #1958
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck ["Pup"]}
                 :runner {:deck ["Employee Strike" (qty "Scrubbed" 3) "Sure Gamble"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (damage state :corp :net 1)
      (is (no-prompt? state :corp) "No choice because of Employee Strike")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Scrubbed")
      (damage state :corp :net 1)
      (is (utils/same-card? (:card (prompt-map :corp)) (:identity (get-corp))) "Employee Strike out of play - Ability turned on correctly")))

(deftest chronos-protocol-selective-mind-mapping-doesn-t-prompt-when-runner-s-hand-is-empty
    ;; Doesn't prompt when Runner's hand is empty
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Neural EMP"]}
                 :runner {:discard ["Sure Gamble"]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "Neural EMP")
      (is (no-prompt? state :corp) "No choice because grip is empty")
      (is (= :corp (:winner @state)))))

(deftest earth-station-sea-headquarters-front-side-additional-cost-to-run-hq
      ;; Additional cost to run HQ
    (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (take-credits state :corp)
        (is (changed? [(:credit (get-runner)) -1]
              (run-on state :hq))
            "Paid 1c to run on HQ")
        (is (last-log-contains? state "spends [Click] and pays 1 [Credits] to make a run on HQ") "Should have correct log with credits price for the run")))

(deftest earth-station-sea-headquarters-front-side-flipping-costs-1-click
      ;; Flipping costs 1 click
    (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (is (changed? [(:click (get-corp)) -1]
              (card-ability state :corp (get-in @state [:corp :identity]) 0))
            "Paid 1 click to flip Earth Station")
        (is (:flipped (get-in @state [:corp :identity])) "Earth Station is on flip side")
        (is (last-log-contains? state "Corp spends [Click] to use Earth Station: SEA Headquarters to flip their identity to Earth Station: Ascending to Orbit.") "Should have correct log with click price")))

(deftest earth-station-sea-headquarters-flip-side-no-additional-cost-to-run-hq
      ;; No additional cost to run HQ
    (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (take-credits state :corp)
        (is (changed? [(:credit (get-runner)) 0]
              (run-on state :hq))
            "Paid nothing to run on HQ")))

(deftest earth-station-sea-headquarters-flip-side-can-t-use-ability-to-flip-back-to-front-side
      ;; Can't use ability to flip back to front side
    (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (is (:flipped (get-in @state [:corp :identity])) "Earth Station is on flip side")
        (is (changed? [(:click (get-corp)) 0]
              (card-ability state :corp (get-in @state [:corp :identity]) 0))
            "Paid 1 click to flip Earth Station")
        (is (:flipped (get-in @state [:corp :identity])) "Earth Station is still on flip side")))

(deftest earth-station-sea-headquarters-flip-side-additional-cost-to-run-a-remote
      ;; Additional cost to run a remote
    (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"
                          :deck ["PAD Campaign"]}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (is (changed? [(:credit (get-runner)) -6]
              (run-on state :remote1))
            "Paid 6c to run on remote server")
        (is (last-log-contains? state "spends [Click] and pays 6 [Credits] to make a run on Server 1") "Should have correct log with credits price for the run")))

(deftest earth-station-sea-headquarters-flip-side-flip-back-on-successful-hq-run
      ;; Flip back on successful HQ run
    (do-game
        (new-game {:corp {:id "Earth Station: SEA Headquarters"
                          :deck ["PAD Campaign"]}})
        (card-ability state :corp (get-in @state [:corp :identity]) 0)
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (is (:flipped (get-in @state [:corp :identity])) "Corp ID is on the flip side")
        (is (changed? [(:credit (get-runner)) 0]
              (run-empty-server state :hq))
            "Paid nothing to run on HQ")
        (is (not (:flipped (get-in @state [:corp :identity]))) "Corp ID is on the front side")))

(deftest earth-station-sea-headquarters-cannot-install-more-than-one-remote
    ;; Cannot install more than one remote
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :deck [(qty "PAD Campaign" 2)]}})
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (= 1 (count (core/get-remotes state))) "Could not install second remote")))

(deftest earth-station-sea-headquarters-creating-more-servers-while-the-identity-is-disabled
    ;; Creating more servers while the identity is disabled
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

(deftest earth-station-sea-headquarters-rules-corner-case-architects-on-non-saved-remotes-can-not-be-trashed
    ;; Rules corner case: Architects on non-saved remotes can not be trashed
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :hand [(qty "Architect" 2) "Project Atlas" (qty "PAD Campaign" 2)]}
                 :runner {:deck ["Employee Strike"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (dotimes [_ 2] (play-from-hand state :corp "Architect" "New remote"))
      (rez state :corp (get-ice state :remote1 0))
      (rez state :corp (get-ice state :remote2 0))
      (play-and-score state "Project Atlas")
      (click-prompt state :corp "Server 2")
      (is (= 1 (count (:discard (get-corp)))) "One of the Architects was trashed, since it gets trashed by the game and not by players")))

(deftest earth-station-sea-headquarters-worlds-plaza-interaction-issue-4723
    ;; Worlds Plaza interaction. Issue #4723
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :hand ["Worlds Plaza" "NASX"]}})
      (play-from-hand state :corp "Worlds Plaza" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (card-ability state :corp (get-content state :remote1 0) 0)
      (click-card state :corp "NASX")
      (is (= "NASX" (:title (first (:hosted (get-content state :remote1 0))))))))

(deftest earth-station-sea-headquarters-full-immersion-recstudio-interaction-issue-4723
    ;; Full Immersion RecStudio interaction. Issue #4723
    (do-game
      (new-game {:corp {:id "Earth Station: SEA Headquarters"
                        :hand ["Full Immersion RecStudio" "NASX"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (card-ability state :corp (get-content state :remote1 0) 0)
      (click-card state :corp "NASX")
      (is (= "NASX" (:title (first (:hosted (get-content state :remote1 0))))))))

(deftest edward-kim-humanity-s-hammer-trash-first-operation-accessed-each-turn-but-not-if-first-one-was-in-archives
    ;; Trash first operation accessed each turn, but not if first one was in Archives
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
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))
      (is (nil? (get-run)))))

(deftest edward-kim-humanity-s-hammer-interaction-with-eater
    ;; Interaction with Eater
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
        (rez state :corp (get-ice state :archives 0))
        (run-continue state)
        (card-ability state :runner eater 0)
        (click-prompt state :runner "End the run")
        (run-continue state)
        (run-continue state)
        (is (= 1 (count (:discard (get-corp)))))
        (run-empty-server state "HQ")
        (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))

(deftest edward-kim-humanity-s-hammer-do-not-trigger-maw-on-first-operation-access-due-to-trash
    ;; Do not trigger Maw on first Operation access (due to trash)
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

(deftest edward-kim-humanity-s-hammer-do-not-trigger-trash-with-divide-and-conquer
    ;; Do not trigger trash with Divide and Conquer
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3)]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Divide and Conquer" (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (is (= 1 (count (:discard (get-corp)))) "Only Hedge Fund in archives")
      (play-from-hand state :runner "Divide and Conquer")
      (run-continue state)
      (is (= 1 (count (:discard (get-corp)))) "Still only Hedge Fund in archives")))

(deftest edward-kim-humanity-s-hammer-trashing-an-operation-not-during-a-run-won-t-create-a-run-issue-3399
    ;; Trashing an operation not during a run won't create a run. Issue #3399
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hostile Takeover"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :hand ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))
      (is (nil? (get-run)) "No run has been created")))

(deftest edward-kim-humanity-s-hammer-interaction-with-aumakua-and-accessing-an-operation-in-archives-5054
    ;; Interaction with Aumakua and accessing an operation in archives #5054
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :discard [(qty "Hedge Fund" 2)]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :hand ["Aumakua"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (get-program state 0) :virus)))))

(deftest edward-kim-humanity-s-hammer-trashed-card-is-logged
    ;; Trashed card is logged
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (is (last-log-contains? state "Runner uses Edward Kim: Humanity's Hammer to trash Hedge Fund at no cost."))))

(deftest esa-afontov-eco-insurrectionist
  (testing "happy path"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 2)]}
                 :runner {:id "Esâ Afontov: Eco-Insurrectionist"
                          :hand [(qty "Amped Up" 5)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Amped Up")
      (click-prompt state :runner "Yes")
      (is (last-log-contains? state "uses Esâ Afontov: Eco-Insurrectionist to sabotage 2") "Sabotage happened")
      (is (prompt-is-type? state :corp :select) "Corp has sabotage prompt")))
  (testing "Does not trigger on second time"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 2)]}
                 :runner {:id "Esâ Afontov: Eco-Insurrectionist"
                          :hand [(qty "Amped Up" 5)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Amped Up")
      (is (= "Draw 1 card?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= "Choose up to 2 cards to trash from HQ. Remainder will be trashed from top of R&D."
             (:msg (prompt-map :corp))))
      (is (last-log-contains? state "uses Esâ Afontov: Eco-Insurrectionist to sabotage 2") "Sabotage happened")
      (click-card state :corp (first (:hand (get-corp))))
      (click-prompt state :corp "Done")
      (play-from-hand state :runner "Amped Up")
      (is (not (last-log-contains? state "uses Esâ Afontov: Eco-Insurrectionist to sabotage 2")) "Sabotage did not happen")
      (is (no-prompt? state :corp) "no Corp prompt")))
  (testing "Plays nicely with Longevity Serum"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 5)]
                        :hand ["Hedge Fund" "Hostile Takeover" "Longevity Serum" "Project Atlas"]
                        :discard ["Prisec"]}
                 :runner {:id "Esâ Afontov: Eco-Insurrectionist"
                          :hand ["Marrow" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Marrow")
      (click-prompt state :runner "No")
      (click-prompt state :corp "Done")
      (take-credits state :runner)
      (play-and-score state "Longevity Serum")
      (is (= "Choose any number of cards in HQ to trash"
             (:msg (prompt-map :corp))))
      (click-card state :corp "Hedge Fund")
      (click-card state :corp "Hostile Takeover")
      (click-prompt state :corp "Done")
      (is (= "Choose up to 3 targets for Longevity Serum"
             (:msg (prompt-map :corp))))
      (click-card state :corp "Hostile Takeover")
      (click-card state :corp "Prisec")
      (click-card state :corp "Hedge Fund")
      (is (= "Choose up to 1 card to trash from HQ. Remainder will be trashed from top of R&D."
             (:msg (prompt-map :corp))))
      (click-card state :corp "Project Atlas"))))

(deftest ele-smoke-scovak-cynosure-of-the-net-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:id "Ele \"Smoke\" Scovak: Cynosure of the Net"
                          :deck ["Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Refractor")
      (let [smoke (get-in @state [:runner :identity])
            refr (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner refr 1)
              (click-card state :runner smoke))
            "Used 1 credit from Smoke"))))

(deftest epiphany-analytica-nations-undivided
  (do-game
    (new-game {:corp {:id "Epiphany Analytica: Nations Undivided"
                        :hand ["Project Atlas" "Rashida Jaheem" "Accelerated Beta Test" "Brainstorm" "Chiyashi"]}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (play-from-hand state :corp "Project Atlas" "New remote")
    (take-credits state :corp)
    (let [epiph (get-in @state [:corp :identity])]
      (is (changed? [(get-counters (refresh epiph) :power) 1]
            (run-empty-server state "Server 1")
            (click-prompt state :runner "Pay 1 [Credits] to trash"))
          "Got 1 power counter")
      (is (changed? [(get-counters (refresh epiph) :power) 0]
            (run-empty-server state "Server 2")
            (click-prompt state :runner "Steal"))
          "Got no additional power counters")
      (take-credits state :runner)
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      ;; R&D is now from top to bottom: A B C D
      (is (changed? [(get-counters (refresh epiph) :power) -1]
            (card-ability state :corp (:identity (get-corp)) 0)
            (is (str/includes? (:msg (prompt-map :corp)) "Accelerated Beta Test, Brainstorm, and Chiyashi"))
            (click-prompt state :corp "OK")
            (click-prompt state :corp "Brainstorm")
            (click-prompt state :corp "HQ"))
          "Spend hosted power counters")
      (is (= "Brainstorm" (:title (get-ice state :hq 0))) "Brainstorm is installed")
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Chiyashi")))))

(deftest epiphany-analytica-nations-undivided-declines
  (do-game
    (new-game {:corp {:id "Epiphany Analytica: Nations Undivided"
                        :hand ["Rashida Jaheem" "Ad Blitz" "Biased Reporting" "Celebrity Gift"]}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (take-credits state :runner)
    (core/move state :corp (find-card "Ad Blitz" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Biased Reporting" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Celebrity Gift" (:hand (get-corp))) :deck)
    (card-ability state :corp (:identity (get-corp)) 0)
    (is (str/includes? (:msg (prompt-map :corp)) "Ad Blitz, Biased Reporting, and Celebrity Gift"))
    (click-prompt state :corp "OK")
    (click-prompt state :corp "Cancel")
    (is (no-prompt? state :corp))
    (is (= (:title (nth (-> @state :corp :deck) 0)) "Ad Blitz"))
    (is (= (:title (nth (-> @state :corp :deck) 1)) "Biased Reporting"))
    (is (= (:title (nth (-> @state :corp :deck) 2)) "Celebrity Gift"))))

(deftest exile-streethawk-simultaneous-resolution-prompt-shown-for-interaction-with-customized-secretary
    ;; Simultaneous-resolution prompt shown for interaction with Customized Secretary
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

(deftest exile-streethawk-interaction-with-harbinger
    ;; Interaction with Harbinger
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
        (rez state :corp grim)
        (run-continue state)
        (card-subroutine state :corp (refresh grim) 0)
        (click-card state :corp harb)
        (is (= 0 (count (:hand (get-runner)))) "Exile didn't draw a card"))))

(deftest freedom-khumalo-crypto-anarchist-only-works-with-assets-ice-operations-and-upgrades
    ;; Only works with Assets, ice, Operations, and Upgrades
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

(deftest freedom-khumalo-crypto-anarchist-triggers-when-play-rez-cost-less-than-or-equal-to-number-of-available-virus-counters
    ;; Triggers when play/rez cost less than or equal to number of available virus counters
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

(deftest freedom-khumalo-crypto-anarchist-doesn-t-trigger-when-there-aren-t-enough-available-virus-counters
    ;; Doesn't trigger when there aren't enough available virus counters
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

(deftest freedom-khumalo-crypto-anarchist-can-use-multiple-programs-for-virus-counter-payment
    ;; Can use multiple programs for virus counter payment
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

(deftest freedom-khumalo-crypto-anarchist-can-use-viruses-on-hosted-cards
    ;; Can use viruses on hosted cards
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

(deftest freedom-khumalo-crypto-anarchist-doesn-t-trigger-when-accessing-an-agenda
    ;; Doesn't trigger when accessing an Agenda
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= ["Steal"] (prompt-buttons :runner)) "Only option should be 'Steal'")))

(deftest freedom-khumalo-crypto-anarchist-shows-multiple-prompts-when-playing-imp
    ;; Shows multiple prompts when playing Imp
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

(deftest freedom-khumalo-crypto-anarchist-should-return-to-access-prompts-when-done-is-pressed
    ;; Should return to access prompts when Done is pressed
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

(deftest freedom-khumalo-crypto-anarchist-shouldn-t-grant-additional-accesses-after-trashing-accessed-card-3423
    ;; Shouldn't grant additional accesses after trashing accessed card. #3423
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

(deftest freedom-khumalo-crypto-anarchist-shouldn-t-give-aumakua-additional-counters-on-trash-3479
    ;; Shouldn't give Aumakua additional counters on trash. #3479
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

(deftest freedom-khumalo-crypto-anarchist-interaction-with-trash-cost-bonuses-and-declining-ability-once-initiated
    ;; interaction with trash-cost-bonuses, and declining ability once initiated
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

(deftest freedom-khumalo-crypto-anarchist-doesn-t-trigger-when-cerberal-static-installed
    ;; Doesn't trigger when Cerberal Static installed
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Cerebral Static"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (play-from-hand state :corp "Cerebral Static")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= 1 (count (prompt-buttons :runner))) "Should only have 1 option")
      (is (= ["No action"] (prompt-buttons :runner)) "Only option should be 'Done'")))

(deftest freedom-khumalo-crypto-anarchist-cannot-remove-virus-counters-from-corp-cards
    ;; Cannot remove virus counters from Corp cards
    (do-game
      (new-game {:runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}
                 :corp {:deck ["Sandstone" "Ice Wall"]}})
      (play-from-hand state :corp "Sandstone" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (let [sandstone (get-ice state :rd 0)]
        (rez state :corp sandstone)
        (run-on state "R&D")
        (run-continue state)
        (card-subroutine state :corp sandstone 0)
        (is (not (:run @state)) "Run Ended")
        (run-empty-server state "HQ")
        (click-prompt state :runner "[Freedom Khumalo] Trash card")
        (is (= 1 (get-counters (refresh sandstone) :virus)) "Sandstone has 1 virus counter")
        (click-card state :runner (refresh sandstone))
        (is (= 1 (get-counters (refresh sandstone) :virus)) "Sandstone should still have 1 virus counter"))))

(deftest freedom-khumalo-crypto-anarchist-ability-activation-trigger-only-counts-runner-virus-counters
    ;; Ability activation trigger only counts Runner virus counters
    (do-game
      (new-game {:runner {:id "Freedom Khumalo: Crypto-Anarchist"}
                 :corp {:deck ["Sandstone" "Ice Wall"]}})
      (play-from-hand state :corp "Sandstone" "R&D")
      (let [sandstone (get-ice state :rd 0)]
        (rez state :corp sandstone)
        (core/add-counter state :corp sandstone :virus 1)
        (take-credits state :corp)
        (run-empty-server state "HQ")
        (is (= 1 (get-counters (refresh sandstone) :virus)) "Sandstone has 1 virus counter")
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
    (is (no-prompt? state :runner) "Runner cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprice") "Accessed card name was logged")))

(deftest gamenet-where-dreams-are-real-gain-credits-from-gold-farmer
    ;; Gain credits from gold farmer
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real" :hand ["Gold Farmer"]}
                 :runner {:hand ["Corroder"] :credits 10}})
      (play-from-hand state :corp "Gold Farmer" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 2]
            (auto-pump-and-break state (get-program state 0)))
          "Corp gains credits when gold farmer is broken")))

(deftest gamenet-where-dreams-are-real-gain-credits-from-f2p
    ;; Gain credits from F2P
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real" :hand ["F2P"]}})
      (play-from-hand state :corp "F2P" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 1]
            (card-side-ability state :runner (get-ice state :hq 0) 0)
            (click-prompt state :runner "Add an installed Runner card to the grip"))
          "Corp gains credits when F2P is used")))

(deftest gamenet-where-dreams-are-real-gain-credits-from-bellona-steal
    ;; Gain credits from Bellona steal
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real"
                        :hand ["Bellona"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 1]
            (click-prompt state :runner "Pay to steal"))
          "Corp gains credits when Bellona is stolen")))

(deftest gamenet-where-dreams-are-real-gain-credits-from-napd-cordon-steal
    ;; Gain credits from NAPD cordon steal
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real"
                        :hand ["Send a Message" "NAPD Cordon"]}})
      (play-from-hand state :corp "NAPD Cordon")
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 1]
            (click-prompt state :runner "Pay to steal"))
          "Corp gains credits when agenda is stolen with NAPD Cordon")))

(deftest gamenet-where-dreams-are-real-gain-credits-from-traces
    ;; Gain credits from traces
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real" :hand ["Uroboros"]}})
      (play-from-hand state :corp "Uroboros" "HQ")
      (let [uroboros (get-ice state :hq 0)]
        (take-credits state :corp)
        (run-on state "HQ")
        (rez state :corp uroboros)
        (run-continue state)
        (is (changed? [(:credit (get-corp)) 1]
              (card-subroutine state :corp (refresh uroboros) 0)
              (click-prompt state :corp "0")
              (click-prompt state :runner "1"))
            "Corp gains credits when a trace is paid into"))))

(deftest gamenet-where-dreams-are-real-gain-credits-from-psi-games
    ;; Gain credits from psi games
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real"
                        :hand ["Caprice Nisei"]}})
      (play-from-hand state :corp "Caprice Nisei" "New remote")
      (let [caprice (get-content state :remote1 0)]
        (take-credits state :corp)
        (rez state :corp caprice)
        (run-on state "Server 1")
        (is (changed? [(:credit (get-corp)) 1]
              (click-prompt state :corp "0 [Credits]")
              (click-prompt state :runner "1 [Credits]"))
            "Corp gains credits when a psi game is paid into"))))

(deftest gamenet-where-dreams-are-real-no-credits-from-trashing
    ;; No credits from trashing
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real" :hand ["PAD Campaign"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 0]
            (click-prompt state :runner "Pay 4 [Credits] to trash"))
          "Corp gains no credits when a trash cost is paid")))

(deftest gamenet-where-dreams-are-real-no-credits-from-runner-cards
    ;; No credits from runner cards
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real"}
                 :runner {:hand ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (is (changed? [(:credit (get-corp)) 0]
            (card-ability state :runner (get-program state 0) 1))
          "Corp gains no credits when an icebreaker is used")))

(deftest gamenet-where-dreams-are-real-no-credits-from-the-source
    ;; No credits from the source
    (do-game
      (new-game {:corp {:id "GameNET: Where Dreams are Real"
                        :hand ["Send a Message"]}
                 :runner {:hand ["The Source"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Source")
      (run-on state "HQ")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) 0]
            (click-prompt state :runner "Pay to steal"))
          "Corp gains no credits when the source's additional cost is paid")))

(deftest gamenet-no-trigger-when-runner-at-zero-credits
  ;; no trigger when runner at zero credits
  (do-game
   (new-game {:corp {:id "GameNET: Where Dreams are Real"
                     :hand ["Rime"]}
              :runner {:credits 0}})
   (play-from-hand state :corp "Rime" "HQ")
   (take-credits state :corp)
   (run-on state "HQ")
   (let [rime (get-ice state :hq 0)]
     (rez state :corp rime)
     (run-continue state :encounter-ice)
     (is (changed? [(:credit (get-corp)) 0]
           (card-subroutine state :corp (refresh rime) 0))
         "Corp does not gain credits when the Runner loses zero credits"))))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (count-bad-pub state)) "GRNDL starts with 1 bad publicity")))

(deftest grndl-power-unleashed-vs-valencia-only-1-bad-pub-at-start
    ;; vs Valencia - only 1 bad pub at start
    (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (count-bad-pub state)) "GRNDL starts with 1 bad publicity")))

(deftest haarpsichord-studios-entertainment-unleashed-prevent-stealing-more-than-1-agenda-per-turn
    ;; Prevent stealing more than 1 agenda per turn
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

(deftest haarpsichord-studios-entertainment-unleashed-interactions-with-employee-strike-issue-1313
    ;; Interactions with Employee Strike. Issue #1313
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
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")
      (is (= 2 (:agenda-point (get-runner))) "Third steal prevented")))

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
    (rez state :corp (get-ice state :archives 1))
    (run-continue-until state :approach-ice)
    (rez state :corp (get-ice state :archives 0))
    (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (run-continue state)
    (click-card state :corp (get-ice state :hq 0))
    (is (= 3 (:credit (get-corp))) "Corp not charged for Architects of Tomorrow rez of Eli 1.0")
    (is (rezzed? (get-ice state :hq 0)) "Eli 1.0 is rezzed")))

(deftest haas-bioroid-architects-of-tomorrow-ice-destruction-in-hb-architects-of-tomorrow-5514
    ;; Ice Destruction in HB Architects of Tomorrow #5514
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Architects of Tomorrow"
                        :deck [(qty "Eli 1.0" 3)]
                        :credits 10}
                 :runner {:deck ["Chisel" "Devil Charm"]}})
      (play-from-hand state :corp "Eli 1.0" "HQ")
      (play-from-hand state :corp "Eli 1.0" "HQ")
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Devil Charm")
      (play-from-hand state :runner "Chisel")
      (click-card state :runner (get-ice state :hq 1))
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (click-prompt state :runner "Devil Charm")
      (click-prompt state :runner "Yes")
      (run-continue state :approach-ice)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (not (no-prompt? state :runner)) "Architects of Tomorrow is still available")))

(deftest haas-bioroid-engineering-the-future-interaction-with-employee-strike
    ;; interaction with Employee Strike
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
      (is (= 9 (:credit (get-corp))) "Corp gained 1cr from EtF")))

(deftest haas-bioroid-precision-design
  ;; Haas-Bioroid: Precision Design
  (do-game
      (new-game {:corp {:id "Haas-Bioroid: Precision Design"
                        :hand ["Project Vitruvius"]
                        :discard ["Hedge Fund"]}})
      (is (= 6 (hand-size :corp)) "Max hand size is 6")
      (play-and-score state "Project Vitruvius")
      (is (= 1 (count (:discard (get-corp)))) "1 card in archives")
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Hedge Fund" (:discard (get-corp)))) ; Ability target
      (is (= 0 (count (:discard (get-corp)))) "0 card in archives")))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game {:corp {:id "Haas-Bioroid: Stronger Together"
                      :deck ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (rez state :corp eli)
      (is (= 5 (get-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest hayley-kaplan-universal-scholar
  (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :hand ["Corroder" "Cache" (qty "Fan Site" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Cache" (:hand (get-runner))))
      (is (= 2 (count (:hand (get-runner)))) "Installed Corroder and Cache.")
      (play-from-hand state :runner "Fan Site")
      (is (no-prompt? state :runner) "No Hayley prompt if not first install this turn.")))

(deftest hayley-kaplan-universal-scholar-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :hand ["Corroder" "Sahasrara"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sahasrara")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
      (let [rara (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (click-card state :runner rara)
              (click-card state :runner rara))
            "Used 2 credits from Sahasrara to install Corroder"))
      (is (empty? (:hand (get-runner))) "Installed Sahasrara and Corroder.")))

(deftest hayley-kaplan-universal-scholar-fake-prompt-when-nothing-to-install
    ;; Fake prompt when nothing to install
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
      (is (no-prompt? state :corp) "No Hayley wait prompt if not first install this turn.")
      (is (no-prompt? state :runner) "No Hayley prompt if not first install this turn.")))

(deftest hayley-kaplan-universal-scholar-facedown-installs-do-not-prompt-for-hayley-install
    ;; Facedown installs do not prompt for Hayley install
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
      (is (no-prompt? state :corp) "No Hayley wait prompt for facedown installs.")))

(deftest hoshiko-shiro-untold-protagonist-id-ability
    ;; ID ability
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

(deftest hoshiko-shiro-untold-protagonist-interaction-with-eater
    ;; Interaction with Eater
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
        (rez state :corp van)
        (run-continue state)
        (card-ability state :runner eat 0)
        (click-prompt state :runner "End the run")
        (run-continue state)
        (run-continue state)
        (take-credits state :runner)
        (is (not (:flipped (refresh ho))) "Hoshiko does not flip"))))

(deftest hoshiko-shiro-untold-protagonist-changing-link-and-subtype-when-flipping
    ;; Changing link and subtype when flipping
    (do-game
      (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"}})
      (take-credits state :corp)
      (let [ho (get-in @state [:runner :identity])]
        (is (not (:flipped (refresh ho))) "Hoshiko starts unflipped")
        (is (= 0 (get-link state)) "Hoshiko starts with 0 link")
        (is (has-subtype? (refresh ho) "Natural") "Hoshiko starts with subtype Natural")
        (run-empty-server state :hq)
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (is (:flipped (refresh ho)) "Hoshiko is flipped")
        (is (= 1 (get-link state)) "Hoshiko now has 1 link")
        (is (has-subtype? (refresh ho) "Digital") "Hoshiko now has the subtype Digital"))))

(deftest hoshiko-shiro-untold-protagonist-rebirth-while-flipped-resets-link-5289
    ;; Rebirth while flipped resets link #5289
    (do-game
      (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"
                          :hand ["Rebirth"]}})
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Rebirth")
      (click-prompt state :runner "Alice Merchant: Clan Agitator")
      (is (zero? (get-link state)) "Runner has 0 link")))

(deftest hoshiko-shiro-untold-protagonist-interaction-with-dreamnet
    ;; Interaction with DreamNet
    (do-game
      (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"
                          :deck [(qty "Sure Gamble" 5)]
                          :hand ["DreamNet"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DreamNet")
      (let [ho (get-in @state [:runner :identity])]
        (is (not (:flipped (refresh ho))) "Hoshiko starts unflipped")
        (is (= 0 (get-link state)) "Hoshiko starts with 0 link")
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
        (is (= 1 (get-link state)) "Hoshiko now has 1 link")
        (is (has-subtype? (refresh ho) "Digital") "Hoshiko now has the subtype Digital")
        (let [cards (count (:hand (get-runner)))
              credits (:credit (get-runner))]
          (run-empty-server state :hq)
          (is (= (inc cards) (count (:hand (get-runner)))) "Runner has drawn 1 card on successful run")
          (is (= (inc credits) (:credit (get-runner))) "Runner has gained 1 credit on successful run")))))

(deftest hoshiko-shiro-untold-protagonist-citadel-interaction
  ;; Citadel interaction
    (do-game
   (new-game {:runner {:id "Hoshiko Shiro: Untold Protagonist"
                       :deck ["Citadel Sanctuary"]}
              :corp {:hand ["SEA Source"]
                     :deck [(qty "Hedge Fund" 5)]}})
   (take-credits state :corp)
   (let [ho (get-in @state [:runner :identity])]
     (play-from-hand state :runner "Citadel Sanctuary")
     (run-on state "Archives")
     (run-continue state)
     (take-credits state :runner)
     (play-from-hand state :corp "SEA Source")
     (click-prompt state :corp "0")
     (click-prompt state :runner "0")
     (take-credits state :corp)
     (run-empty-server state :hq)
     (click-prompt state :runner "No action")
     (end-turn state :runner)
     (is (= 0 (get-link state)) "Hoshiko is not flipped yet")
     (click-prompt state :runner "Hoshiko Shiro: Untold Protagonist")
     (is (:flipped (refresh ho)) "Only Hoshiko flip has been resolved")
     (is (= 1 (get-link state)) "Flipped Hoshiko has 1 Link")
     (is (= 1 (:link (prompt-map :corp))) "Trace link should be 1")
     (click-prompt state :corp "0")
     (click-prompt state :runner "0")
     (is (:flipped (refresh ho)) "All end of turn effects have been resolved including a flip")
     (is (= 1 (get-link state)) "Flipped Hoshiko has 1 Link"))))

(deftest hyoubu-institute-absolute-clarity-id-abilities
    ;; ID abilities
    (do-game
     (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"}
                :runner {:deck [(qty "Sure Gamble" 1)]
                         :hand [(qty "Sure Gamble" 2)]}})
     (let [hy (get-in @state [:corp :identity])]
       (dotimes [i 2]
         (is (changed? [(:credit (get-corp)) 1]
               (card-ability state :corp hy i))) ; triggering ID ability triggers a reveal so gains a cred
         (is (changed? [(:credit (get-corp)) 0]
               (card-ability state :corp hy i))) ; triggering ID ability second time in a turn doesn't
         (is (= 1 (:click (get-corp))) "Using ability twice cost 2 clicks")
         (take-credits state :corp)
         (take-credits state :runner))
       (core/move state :runner (first (:deck (get-runner))) :hand)
       (is (= 0 (count (:deck (get-runner)))) "Runner deck is empty")
       (is (changed? [(:credit (get-corp)) 0]
             (card-ability state :corp hy 0)))  ; stack is empty, so revealing nothing gains nothing
       (take-credits state :corp)
       (dotimes [_ 3]
         (play-from-hand state :runner "Sure Gamble"))
       (take-credits state :runner)
       (is (= 0 (count (:hand (get-runner)))) "Runner hand is empty")
       (is (changed? [(:credit (get-corp)) 0]
             (card-ability state :corp hy 1))))))

(deftest hyoubu-institute-absolute-clarity-estrike-interaction
    ;; EStrike interaction
    (do-game
     (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                       :deck ["Scarcity of Resources" "Celebrity Gift"]}
                :runner {:deck [(qty "Employee Strike" 3) "Enhanced Vision"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Enhanced Vision")
     (is (changed? [(:credit (get-corp)) 1]
           (run-empty-server state "Archives")))
     (take-credits state :runner)
     (take-credits state :corp)
     (play-from-hand state :runner "Employee Strike")
     (is (changed? [(:credit (get-corp)) 0]
           (run-empty-server state "Archives")))
     (take-credits state :runner)
     (core/gain state :corp :click 3)
     (play-from-hand state :corp "Celebrity Gift")
     (is (changed? [(:credit (get-corp)) 2]
           ; get 2 creds from celeb gift, but nothing from hyoubu trigger due to estrike
           (click-card state :corp (first (:hand (get-corp))))
           (click-prompt state :corp "Done")))
     (play-from-hand state :corp "Scarcity of Resources")
     (is (changed? [(:credit (get-corp)) 0]
           (card-ability state :corp (get-in @state [:corp :identity]) 1)))))

(deftest hyoubu-institute-absolute-clarity-slot-machine-grail-reflection-fast-track
    ;; Slot Machine, grail, Reflection, Fast Track
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
       (rez state :corp gal)
       (run-continue state)
       (card-ability state :corp (refresh gal) 0) ;reveal grail ice
       (click-card state :corp (find-card "Galahad" (:hand (get-corp))))
       (is (changed? [(:credit (get-corp)) 1]
             (click-prompt state :corp "Done")))
       (is (changed? [(:credit (get-corp)) 0]
             (run-continue state :movement)
             (run-jack-out state))) ; triggers reflection, but trigger already done this turn
       (take-credits state :runner)
       (take-credits state :corp)
       (run-on state "R&D")
       (rez state :corp sm)
       (is (changed? [(:credit (get-corp)) 0]
             (run-continue state))) ;trigger slot machine
       (run-continue state :movement)
       (run-jack-out state)
       (take-credits state :runner)
       (take-credits state :corp)
       (run-on state "Archives")
       (is (changed? [(:credit (get-corp)) 1]
             (run-jack-out state))) ; triggers reflection
       (take-credits state :runner)
       (core/move state :corp (find-card "House of Knives" (:hand (get-corp))) :deck)
       (play-from-hand state :corp "Fast Track")
       (is (changed? [(:credit (get-corp)) 1]
             (click-prompt state :corp "House of Knives"))))))

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
  (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck [(qty "PAD Campaign" 3) (qty "Hedge Fund" 3)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (let [pad (get-content state :remote1 0)]
        (rez state :corp pad)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (= 8 (core/trash-cost state :runner (refresh pad)))))))

(deftest industrial-genomics-growing-solutions-with-product-recall
    ;; with Product Recall
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck ["Product Recall" (qty "PAD Campaign" 3) (qty "Hedge Fund" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (let [pad (get-content state :remote1 0)]
        (rez state :corp pad)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (= 8 (core/trash-cost state :runner (refresh pad))))
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (play-from-hand state :corp "Product Recall")
        (let [credits (:credit (get-corp))]
          (click-card state :corp pad)
          (is (= (+ credits 8) (:credit (get-corp))) "Gain 8 credits from trashing PAD Campaign")))))

(deftest issuaq-adaptics-normal-score
  ;; Issuaq Adaptics - does not trigger when scoring normally
  (do-game
    (new-game {:corp {:id "Issuaq Adaptics: Sustaining Diversity"
                      :deck ["House of Knives"]}})
    (play-from-hand state :corp "House of Knives" "New remote")
    (let [issuaq (get-in @state [:corp :identity])
          hok (get-content state :remote1 0)]
      (take-credits state :corp)
      (take-credits state :runner)
      (score-agenda state :corp hok)
      (is (= 0 (get-counters (refresh issuaq) :power)) "Issuaq has no power counters")
      (is (= 7 (:agenda-point-req (get-corp))) "Corp still requires 7 points to win"))))

(deftest issuaq-adaptics-single-score
  ;; Issuaq Adaptics - Adjust point requirement when a single agenda is scored
  (do-game
    (new-game {:corp {:id "Issuaq Adaptics: Sustaining Diversity"
                      :deck ["Project Kusanagi", "Seamless Launch"]}})
    (play-from-hand state :corp "Project Kusanagi" "New remote")
    (let [issuaq (get-in @state [:corp :identity])
          pk (get-content state :remote1 0)]
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Seamless Launch")
      (click-card state :corp pk)
      (score state :corp (refresh pk))
      (is (= 6 (:agenda-point-req (get-corp))) "Corp Agenda point requirement reduced by 1")
      (is (= 1 (get-counters (refresh issuaq) :power)) "Issuaq Adaptics has 1 power counter"))))

(deftest issuaq-adaptics-multiple-score
  ;; Issuaq Adaptics - Adjusts point requirement after multiple agendas are scored
  (do-game
      (new-game {:corp {:id "Issuaq Adaptics: Sustaining Diversity"
                        :deck [(qty "Project Kusanagi" 2) (qty "Seamless Launch" 2)]}})
      (play-from-hand state :corp "Project Kusanagi" "New remote")
      (play-from-hand state :corp "Project Kusanagi" "New remote")
      (let [issuaq (get-in @state [:corp :identity])
            pk1 (get-content state :remote1 0)
            pk2 (get-content state :remote2 0)]
        (take-credits state :corp)
        (take-credits state :runner)
        (play-from-hand state :corp "Seamless Launch")
        (click-card state :corp pk1)
        (play-from-hand state :corp "Seamless Launch")
        (click-card state :corp pk2)
        (score state :corp (refresh pk1))
        (score state :corp (refresh pk2))
        (is (= 5 (:agenda-point-req (get-corp))) "Corp Agenda point requirement reduced by 2")
        (is (= 2 (get-counters (refresh issuaq) :power)) "Issuaq Adaptics has 2 power counters"))))

(deftest jemison-astronautics-sacrifice-audacity-success
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
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
          (is (no-prompt? state :corp) "No Jemison prompt for Runner forfeit")
          (take-credits state :runner)
          (play-from-hand state :corp "Global Food Initiative" "New remote")
          (score-agenda state :corp (get-content state :remote2 0))
          (rez state :corp enf {:expect-rez false})
          (click-card state :corp (get-in (get-corp) [:scored 0]))
          (click-card state :corp iwall)
          (is (= 4 (get-counters (refresh iwall) :advancement)) "Jemison placed 4 advancements")))))

(deftest jemison-astronautics-sacrifice-audacity-success-gene-splicer
    ;; Gene Splicer
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Ice Wall" "Archer" "Gene Splicer"]
                        :credits 10}})
      (core/gain state :corp :click 3)
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Archer" "HQ")
      (play-from-hand state :corp "Gene Splicer" "New remote")
      (let [gs (get-content state :remote1 0)
            arch (get-ice state :hq 0)
            iwall (get-ice state :rd 0)]
        (core/add-counter state :corp gs :advancement 3)
        (rez state :corp (refresh gs))
        (card-ability state :corp (refresh gs) 0)
        (is (nil? (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
        (is (= 1 (:agendapoints (get-scored state :corp 0))) "Gene Splicer added to Corp score area")
        (rez state :corp arch {:expect-rez false})
        (click-card state :corp (get-in (get-corp) [:scored 0]))
        (is (zero? (:agenda-point (get-corp))) "Gene Splicer agenda points removed")
        (is (= "Gene Splicer" (-> (get-corp) :rfg first :title)) "Gene Splicer should be removed from game")
        (click-card state :corp iwall)
        (is (= 2 (get-counters (refresh iwall) :advancement)) "Jemison placed 2 advancements"))))

(deftest jemison-astronautics-sacrifice-audacity-success-24-7-armed-intimidation-combination
    ;; Expected result: 24/7 causes Forfeit, Jemison places counters, AI triggers
    ;; 24/7 - Armed Intimidation combination
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
      (is (= 4 (count-tags state)) "Runner took 2 more tags from AI -- happens at the end of all the async completion")))

(deftest jesminder-sareen-girl-behind-the-curtain
  ;; Jesminder Sareen - avoid tags only during a run
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
        (rez state :corp dr2)
        (run-continue state)
        (click-prompt state :runner "Take 1 tag")
        (is (zero? (count-tags state)) "Jesminder avoided first tag during the run")
        (run-continue-until state :approach-ice dr1)
        (rez state :corp dr1)
        (run-continue state)
        (click-prompt state :runner "Take 1 tag")
        (is (= 1 (count-tags state)) "Jesminder did not avoid the second tag during the run")
        (run-continue-until state :success)
        (run-empty-server state "R&D") ; clear per-run buffer
        (take-credits state :runner)
        (play-from-hand state :corp "SEA Source")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (count-tags state)) "Jesminder did not avoid the tag outside of a run"))))

(deftest jesminder-sareen-girl-behind-the-curtain-don-t-avoid-john-masanori-tag
    ;; don't avoid John Masanori tag
    (do-game
      (new-game {:runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :deck ["John Masanori"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "John Masanori")
      (run-on state "HQ")
      (run-jack-out state)
      (is (= 1 (count-tags state)) "Jesminder did not avoid John Masanori tag")))

(deftest jinteki-biotech-life-imagined-brewery-net-damage
    ;; Brewery net damage
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Brewery")
      (start-turn state :corp)
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (= 1 (count (:hand (get-runner)))) "Runner took 2 net damage from Brewery flip")))

(deftest jinteki-biotech-life-imagined-greenhouse-four-advancement-tokens
    ;; Greenhouse four advancement tokens
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Greenhouse")
      (start-turn state :corp)
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (let [bt (get-content state :remote1 0)]
        (is (zero? (get-counters (refresh bt) :advancement)) "No advancement counters on agenda")
        (card-ability state :corp (:identity (get-corp)) 1)
        (click-card state :corp (refresh bt))
        (is (= 4 (get-counters (refresh bt) :advancement)) "Four advancement counters on agenda"))))

(deftest jinteki-biotech-life-imagined-tank-shuffle-archives-into-r-d
    ;; Tank shuffle Archives into R&D
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck [(qty "Hedge Fund" 3)]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Tank")
      (start-turn state :corp)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :runner)
      (is (= 3 (count (:discard (get-corp)))) "Archives started with 3 cards")
      (is (zero? (count (:deck (get-corp)))) "R&D started empty")
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (zero? (count (:discard (get-corp)))) "Archives ended empty")
      (is (= 3 (count (:deck (get-corp)))) "R&D ended with 3 cards")))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Take 1 net when an agenda is scored or stolen
  (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Braintrust" 6)]}
                 :runner {:hand [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage from steal")))

(deftest jinteki-personal-evolution-interaction-with-employee-striek-issue-4124
    ;; Interaction with Employee Striek. Issue #4124
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Braintrust" 6)]}
                 :runner {:hand ["Employee Strike" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (= 3 (count (:hand (get-runner)))) "Runner took 0 net damage from steal")))

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
  (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]}})
      (play-from-hand state :corp "Mental Health Clinic" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (run-empty-server state "HQ")
      (is (core/can-run-server? state "Server 1") "Runner can run on remotes")))

(deftest jinteki-replicating-perfection-interaction-with-employee-strike-issue-1313-and-1956
    ;; interaction with Employee Strike. Issue #1313 and #1956.
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
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")))

(deftest jinteki-replicating-nightmare-scenarios
  ;; Replicating Perfection - Prevent runner from running on remotes unless they first run on a central
  ;; also take into account Front Company, Off the Grid, Marathon
  (do-game
    (new-game {:corp {:id "Jinteki: Replicating Perfection"
                      :hand ["Front Company" "Off the Grid" "Rashida Jaheem"]}
               :runner {:hand ["Marathon" "Direct Access"]}})
    (core/gain state :corp :credit 20)
    (play-from-hand state :corp "Front Company" "New remote")
    (play-from-hand state :corp "Off the Grid" "New remote")
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (let [fc (get-content state :remote1 0)
          otg (get-content state :remote2 0)
          rashida (get-content state :remote3 0)]
      (rez state :corp (refresh fc))
      (rez state :corp (refresh otg))
      (rez state :corp (refresh rashida))
      (take-credits state :corp)
      ;; still can't run remotes with direct access
      (is (not (core/can-run-server? state "Server 1")))
      (is (core/can-run-server? state "R&D"))
      (play-from-hand state :runner "Direct Access")
      (is (not (core/can-run-server? state "Server 1")))
      (click-prompt state :runner "R&D")
      (is (= :rd (get-in @state [:run :server 0])) "Running on remote vs RP")
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (= "Direct Access" (-> (get-runner) :deck first :title)) "Direct Access shuffled into stack")
      ;; still can't run on off the grid, but can run other remotes
      (is (not (core/can-run-server? state "Server 2")))
      (is (core/can-run-server? state "Server 1"))
      (is (core/can-run-server? state "Server 3"))
      (run-on state "Server 1")
      (run-continue state :success)
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (play-from-hand state :runner "Marathon")
      (click-prompt state :runner "Server 3")
      (run-jack-out state)
      ;;can't run OTG or Rashida
      (is (not (core/can-run-server? state "Server 2")))
      (is (not (core/can-run-server? state "Server 3")))
      (take-credits state :runner)
      (take-credits state :corp)
      ;;can't run OTG or Rashida
      (is (not (core/can-run-server? state "Server 2")))
      (is (not (core/can-run-server? state "Server 3")))
      (click-draw state :runner)
      (play-from-hand state :runner "Direct Access")
      (is (not (core/can-run-server? state "Server 2")))
      (is (core/can-run-server? state "Server 3") "Can run because of direct access"))))


(deftest jinteki-restoring-humanity
  ;; Jinteki: Restoring Humanity
  (do-game
   (new-game {:corp {:id "Jinteki: Restoring Humanity"
                     :discard ["Neural EMP"]}})
   (take-credits state :corp)
   (is (= 9 (:credit (get-corp))) "Gained a credit for face down card")
   (run-empty-server state "Archives")
   (take-credits state :runner)
   (take-credits state :corp)
   (is (= 12 (:credit (get-corp))) "Did not gain a credit for face down card")))

(deftest kabonesa-wu-netspace-thrillseeker
  ;; Kabonesa Wu
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
      (is (= "Gordian Blade" (-> (get-runner) :rfg last :title)) "Kabonesa Wu should rfg card installed with ability"))
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
      (click-prompt state :runner "Chaos Theory: Wünderkind")
      (is (= "Chaos Theory: Wünderkind" (:title (:identity (get-runner)))) "Runner is now Chaos Theory")
      (take-credits state :runner)
      (is (nil? (get-program state 0)) "Gordian Blade shouldn't be installed anymore")
      (is (= "Gordian Blade" (-> (get-runner) :rfg last :title))
          "Kabonesa Wu should rfg card installed with ability even tho runner is now a different identity")))

(deftest kate-mac-mccaffrey-digital-tinker-install-discount
    ;; Install discount
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

(deftest kate-mac-mccaffrey-digital-tinker-no-discount-for-0-cost
    ;; No discount for 0 cost
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"
                                 "Self-modifying Code"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Magnum Opus")
      (is (zero? (:credit (get-runner))) "No Kate discount on second program install")))

(deftest kate-mac-mccaffrey-digital-tinker-can-afford-only-with-the-discount
    ;; Can afford only with the discount
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (core/lose state :runner :credit 1)
      (is (= 4 (:credit (get-runner))))
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (count (get-program state))) "Magnum Opus installed")
      (is (zero? (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

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

(deftest khan-savvy-skiptracer-proper-order-of-events-when-vs-caprice
    ;; proper order of events when vs. Caprice
    (do-game
      (new-game {:corp {:deck ["Eli 1.0" "Caprice Nisei"]}
                 :runner {:id "Khan: Savvy Skiptracer"
                          :deck ["Corroder"]}})
      (play-from-hand state :corp "Eli 1.0" "Archives")
      (play-from-hand state :corp "Caprice Nisei" "Archives")
      (rez state :corp (get-content state :archives 0))
      (take-credits state :corp)
      (run-on state "Archives")
      (run-continue state)
      (is (and (= 2 (count (:prompt (get-runner))))
               (= "Khan: Savvy Skiptracer" (-> (prompt-map :runner) :card :title)))
          "Only Khan prompt showing")
      (click-card state :runner "Corroder")
      (is (find-card "Corroder" (-> (get-runner) :rig :program)) "Corroder installed")
      (is (= 4 (:credit (get-runner))) "1cr discount from Khan")
      (is (= "Caprice Nisei" (-> (prompt-map :runner) :card :title)) "Caprice prompt showing")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (is (not (:run @state)) "Run ended")))

(deftest laramy-fisk-savvy-investor-installing-a-shard-should-still-give-option-to-force-corp-draw
    ;; installing a Shard should still give option to force Corp draw
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]
                        :hand [(qty "Hedge Fund" 2) (qty "Eli 1.0" 3)]}
                 :runner {:id "Laramy Fisk: Savvy Investor"
                          :deck ["Eden Shard"]}})
      (take-credits state :corp)
      (run-empty-server state :rd)
      (is (= "Force the Corp to draw 1 card?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= "Choose a breach replacement ability" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Eden Shard") ; Eden Shard's replacement ability
      (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
      (is (= 5 (:credit (get-runner))) "Eden Shard install was free")
      (is (not (:run @state)) "Run ended")
      (is (= 6 (count (:hand (get-corp)))) "Corp forced to draw")))

(deftest lat-ethical-freelancer-ability-fires-draw
    ;; Ability fires - draw
    (do-game
      (new-game {:runner {:id "Lat: Ethical Freelancer"
                          :deck [(qty "Sure Gamble" 6)]}
                 :corp {:deck [(qty "Hedge Fund" 5)]}
                 :options {:start-as :runner}})
      (core/lose state :runner :click 4)
      (end-turn state :runner)
      (is (= "Draw 1 card?" (:msg (prompt-map :runner))))
      (is (= 5 (count (:hand (get-runner)))))
      (is (not (no-prompt? state :corp)) "Corp should have a waiting prompt")
      (click-prompt state :runner "Yes")
      (is (= 6 (count (:hand (get-runner)))))))

(deftest lat-ethical-freelancer-ability-is-interactive
    ;; Chameleon and Lat ability should go along well
    (do-game
      (new-game {:runner {:id "Lat: Ethical Freelancer"
                          :hand ["Chameleon" (qty "Sure Gamble" 5)]
                          :deck ["Sure Gamble"]}
                 :corp {:id "Haas-Bioroid: Precision Design"
                        :hand [(qty "Hedge Fund" 6)]}})
      (take-credits state :corp)
      (core/lose state :runner :click 3)
      (play-from-hand state :runner "Chameleon")
      (click-prompt state :runner "Barrier")
      (end-turn state :runner)
      (is (= 5 (count (:hand (get-runner)))))
      (click-prompt state :runner "Chameleon")
      (is (= "Draw 1 card?" (:msg (prompt-map :runner))))))

(deftest lat-ethical-freelancer-ability-fires-don-t-draw
    ;; Ability fires - don't draw
    (do-game
      (new-game {:runner {:id "Lat: Ethical Freelancer"
                          :deck [(qty "Sure Gamble" 6)]}
                 :corp {:deck [(qty "Hedge Fund" 5)]}
                 :options {:start-as :runner}})
      (core/lose state :runner :click 4)
      (end-turn state :runner)
      (is (= "Draw 1 card?" (:msg (prompt-map :runner))))
      (is (= 5 (count (:hand (get-runner)))))
      (click-prompt state :runner "No")
      (is (= 5 (count (:hand (get-runner)))))))

(deftest lat-ethical-freelancer-ability-doesn-t-fire
    ;; Ability doesn't fire
    (do-game
      (new-game {:runner {:id "Lat: Ethical Freelancer"
                          :deck [(qty "Sure Gamble" 3)]}
                 :corp {:deck [(qty "Hedge Fund" 4)]}
                 :options {:start-as :runner}})
      (core/lose state :runner :click 4)
      (end-turn state :runner)
      (is (no-prompt? state :runner) "No prompt")))

(deftest leela-patel-trained-pragmatist-complicated-interaction-with-mutiple-gang-sign
    ;; complicated interaction with mutiple Gang Sign
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

(deftest leela-patel-trained-pragmatist-issues-with-lingering-successful-run-prompt
    ;; issues with lingering successful run prompt
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

(deftest leela-patel-trained-pragmatist-public-agenda
    ;; agendas with Public subtype are neither rezzed or unrezzed
    (do-game
      (new-game {
                 :corp {:hand ["Ice Wall" "Oaktown Renovation" "Oaktown Renovation"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"}})
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (run-empty-server state :remote2)
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :remote1 0))
      (is (= 0 (count (:hand (get-corp)))) "Leela can not bounce Public agenda")))

(deftest leela-patel-trained-pragmatist-upgrades-returned-to-hand-in-the-middle-of-a-run-do-not-break-the-run-issue-2008
    ;; upgrades returned to hand in the middle of a run do not break the run. Issue #2008
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

(deftest leela-patel-trained-pragmatist
    ;; Leela Patel: Trained Pragmatist
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
      (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand now")))

(deftest liza-talking-thunder-prominent-legislator
  ;; Liza Talking Thunder: Prominent Legislator
  (do-game
      (new-game {:runner {:id "Liza Talking Thunder: Prominent Legislator"
                          :deck [(qty "Sure Gamble" 7)]}})
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (= 7 (count (:hand (get-runner)))) "Drew 2 cards from successful run on Archives")
      (is (= 1 (count-tags state)) "Took 1 tag from successful run on Archives")))

(deftest liza-talking-thunder-prominent-legislator-works-with-crisium-grid
    ;; Works with Crisium Grid
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Crisium Grid"]}
                 :runner {:id "Liza Talking Thunder: Prominent Legislator"
                          :deck [(qty "Sure Gamble" 7)]}})
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (rez state :corp (get-content state :rd 0))
      (take-credits state :corp)
      (is (changed? [(count (:hand (get-runner))) 0]
            (run-empty-server state "R&D"))
          "Crisium blocks ability")
      (is (zero? (count-tags state)) "Took no tags for ability being blocked")))

(deftest los-data-hijacker
  ;; Los: Data Hijacker
  (before-each [state (new-game {:corp {:hand [(qty "Ice Wall" 2)]}
                                 :runner {:id "Los: Data Hijacker"}})]
    (testing "Ability works"
      (do-game state
        (play-from-hand state :corp "Ice Wall" "HQ")
        (is (changed? [(:credit (get-runner)) 2]
              (rez state :corp (get-ice state :hq 0)))
            "Gains 2 from ice rez")))
    (testing "Only works on first rez per turn"
      (do-game state
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Ice Wall" "R&D")
        (rez state :corp (get-ice state :hq 0))
        (is (changed? [(:credit (get-runner)) 0]
              (rez state :corp (get-ice state :rd 0)))
            "Does not gain")))))

(deftest maxx-maximum-punk-rock
  ;; MaxX
  (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Wyldside" 3)
                                 "Eater"]}})
      (starting-hand state :runner ["Eater"])
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (last-log-contains? state "Wyldside and Wyldside")
          "Maxx did log trashed card names")))

(deftest maxx-maximum-punk-rock-with-dummy-box-check-that-mills-don-t-trigger-trash-prevention-3246
    ;; with Dummy Box. Check that mills don't trigger trash prevention #3246
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Dummy Box" 30)]}})
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (play-from-hand state :runner "Dummy Box")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (no-prompt? state :runner) "Dummy Box not fired from mill")))

(deftest maxx-maximum-punk-rock-with-wyldside-using-wyldside-during-step-1-2-should-lose-1-click
    ;; with Wyldside - using Wyldside during Step 1.2 should lose 1 click
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
        (end-phase-12 state :runner)
        (is (= 4 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
        (is (= 3 (:click (get-runner))) "Wyldside caused 1 click to be lost")
        (is (= 3 (count (:hand (get-runner)))) "3 cards drawn total"))))

(deftest mercury-chrome-libertador
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 2)]}
                 :runner {:id "Mercury: Chrome Libertador"}})
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (run-empty-server state :rd)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")))

(deftest mercury-chrome-libertador-no-additional-access-when-subs-are-broken
  (do-game
      (new-game {:corp {:hand [(qty "Hedge Fund" 2) "Ice Wall"]}
                 :runner {:id "Mercury: Chrome Libertador"
                          :hand ["Corroder"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (auto-pump-and-break state (get-program state 0))
      (core/continue state :corp nil)
      (run-continue state :success)
      (click-prompt state :runner "No action")))

(deftest mercury-chrome-libertador-interaction-with-tracker
  (do-game
      (new-game {:corp {:hand [(qty "Hedge Fund" 2) "Ice Wall"]}
                 :runner {:id "Mercury: Chrome Libertador"
                          :hand ["Tracker"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Tracker")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "HQ")
      (card-ability state :runner (get-program state 0) 0)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (fire-subs state (refresh (get-ice state :hq 0)))
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")))

(deftest mirrormorph-endless-iteration-triggers-gain-credit-from-mm
      ;; Gain credit from MM
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Hedge Fund" 10)]}})
        (click-draw state :corp)
        (click-credit state :corp)
        (play-from-hand state :corp "Hedge Fund")
        (is (changed? [(:credit (get-corp)) 5]
              (click-prompt state :corp "Gain 1 [Credits]"))
            "Gained 1 credit from MM ability")))

(deftest mirrormorph-endless-iteration-triggers-gain-click-from-using-asset-ability
      ;; Gain click from using Asset ability
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Capital Investors" 10)]}})
        (click-draw state :corp)
        (play-from-hand state :corp "Capital Investors" "New remote")
        (let [ci (get-content state :remote1 0)]
          (rez state :corp ci)
          (card-ability state :corp (refresh ci) 0)
          (click-prompt state :corp "Gain [Click]")
          (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
          (card-ability state :corp (refresh ci) 0)
          (is (= 1 (:click (get-corp))) "Could not use Capital Investors again with MM click")
          (click-credit state :corp)
          (is (= 0 (:click (get-corp))) "Was able to click for credit"))))

(deftest mirrormorph-endless-iteration-triggers-gain-click-from-using-upgrade-ability
      ;; Gain click from using Upgrade ability
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Cold Site Server" 10)]}})
        (click-draw state :corp)
        (play-from-hand state :corp "Cold Site Server" "New remote")
        (let [css (get-content state :remote1 0)]
          (rez state :corp css)
          (card-ability state :corp (refresh css) 0)
          (click-prompt state :corp "Gain [Click]")
          (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
          (card-ability state :corp (refresh css) 0)
          (is (= 1 (:click (get-corp))) "Could not use Hedge Fund again with MM click")
          (click-credit state :corp)
          (is (= 0 (:click (get-corp))) "Was able to click for credit"))))

(deftest mirrormorph-endless-iteration-triggers-gain-click-from-playing-an-operation
      ;; Gain click from playing an Operation
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Hedge Fund" 10)]}})
        (click-draw state :corp)
        (click-credit state :corp)
        (play-from-hand state :corp "Hedge Fund")
        (click-prompt state :corp "Gain [Click]")
        (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
        (play-from-hand state :corp "Hedge Fund")
        (is (= 1 (:click (get-corp))) "Could not use Hedge Fund again with MM click")))

(deftest mirrormorph-endless-iteration-triggers-gain-click-from-installing-card
      ;; Gain click from installing card
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "PAD Campaign" 10)]}})
        (click-draw state :corp)
        (click-credit state :corp)
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (click-prompt state :corp "Gain [Click]")
        (is (= 1 (:click (get-corp))) "Gained 1 click from MM")
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (is (= 1 (:click (get-corp))) "Could not install another card with MM click")))

(deftest mirrormorph-endless-iteration-triggers-gain-click-from-trashing-three-different-pad-taps
      ;; Gain click from trashing three different PAD Taps
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :credits 9}
                   :runner {:deck [(qty "PAD Tap" 3)]}})
        (take-credits state :corp)
        (dotimes [_ 3] (play-from-hand state :runner "PAD Tap"))
        (take-credits state :runner)
        (let [tap1 (get-resource state 0)
              tap2 (get-resource state 1)
              tap3 (get-resource state 2)]
          (card-side-ability state :corp tap1 0)
          (card-side-ability state :corp tap2 0)
          (card-side-ability state :corp tap3 0)
          (click-prompt state :corp "Gain [Click]")
          (is (= 1 (:click (get-corp))) "Gained 1 click from MM"))))

(deftest mirrormorph-endless-iteration-triggers-trigger-mirrormorph-with-double-operation
      ;; Trigger Mirrormorph with double Operation
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :hand ["Mandatory Upgrades" "Blue Level Clearance"]
                          :deck [(qty "Hedge Fund" 10)]}})
        (play-and-score state "Mandatory Upgrades")
        (take-credits state :corp)
        (take-credits state :runner)
        (click-credit state :corp)
        (click-draw state :corp)
        (play-from-hand state :corp "Blue Level Clearance")
        (is (changed? [(:credit (get-corp)) 4]
              (click-prompt state :corp "Gain 1 [Credits]"))
            "Gained 1 credit from MM ability")))

(deftest mirrormorph-endless-iteration-triggers-trigger-mirrormorph-with-mcaap
      ;; Trigger Mirrormorph with MCAAP
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :hand ["MCA Austerity Policy"]
                          :deck [(qty "Hedge Fund" 10)]}})
        (play-from-hand state :corp "MCA Austerity Policy" "New remote")
        (let [mcaap (get-content state :remote1 0)]
          (rez state :corp mcaap)
          (card-ability state :corp mcaap 0)
          (take-credits state :corp)
          (click-prompt state :corp "Gain 1 [Credits]")
          (take-credits state :runner)
          (card-ability state :corp mcaap 0)
          (take-credits state :corp)
          (take-credits state :runner)
          (card-ability state :corp mcaap 0)
          (click-credit state :corp)
          (card-ability state :corp mcaap 1)
          (is (changed? [(:credit (get-corp)) 1]
                (click-prompt state :corp "Gain 1 [Credits]"))
              "Gained 1 credit from MM ability"))))

(deftest mirrormorph-endless-iteration-does-not-trigger-using-same-asset-ability-multiple-times
      ;; Using same Asset ability multiple times
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Capital Investors" 10)]}})
        (play-from-hand state :corp "Capital Investors" "New remote")
        (let [ci (get-content state :remote1 0)]
          (rez state :corp ci)
          (dotimes [_ 2] (card-ability state :corp (refresh ci) 0))
          (is (no-prompt? state :corp) "No MM trigger"))))

(deftest mirrormorph-endless-iteration-does-not-trigger-using-different-operations
      ;; Using different operations
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :deck [(qty "Hedge Fund" 10)]}})
        (dotimes [_ 3] (play-from-hand state :corp "Hedge Fund"))
        (is (no-prompt? state :corp) "No MM trigger")))

(deftest mirrormorph-endless-iteration-does-not-trigger-installing-different-cards
      ;; Installing different cards
    (do-game
        (new-game {:corp {:id "MirrorMorph: Endless Iteration"
                          :hand ["PAD Campaign" "NASX" "Wall to Wall"]}})
        (play-from-hand state :corp "PAD Campaign" "New remote")
        (play-from-hand state :corp "NASX" "New remote")
        (play-from-hand state :corp "Wall to Wall" "New remote")
        (is (no-prompt? state :corp) "No MM trigger")))

(deftest mti-mwekundu-life-improved-no-ice
    ;; No ice
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (not= "Jack out?" (:msg (prompt-map :runner))) "First approach of ice so no jack out prompt")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))

(deftest mti-mwekundu-life-improved-multiple-ice
    ;; Multiple ice
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma" "Ice Wall" "Bloom"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Bloom" "R&D")
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue-until state :movement (get-ice state :rd 0))
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to jack out")
      (click-prompt state :runner "No")
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))

(deftest mti-mwekundu-life-improved-jack-out
    ;; Jack out
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma" "Ice Wall" "Bloom"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Bloom" "R&D")
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue-until state :movement (get-ice state :rd 0))
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to jack out")
      (click-prompt state :runner "Yes")
      (is (empty? (:run @state)) "Run has ended")
      (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))

(deftest mti-mwekundu-life-improved-with-kakugo-passing-shouldn-t-fire-net-damage-twice-3588
    ;; with Kakugo, passing shouldn't fire net damage twice. #3588
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Kakugo"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Kakugo" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Kakugo" (:title (get-ice state :hq 0))) "Kakugo was installed")
      (is (empty? (:hand (get-corp))) "Kakugo removed from HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue-until state :movement)
      (is (= 1 (-> (get-runner) :discard count)) "Runner should take 1 net damage from Kakugo")))

(deftest mti-mwekundu-life-improved-bogus-prompt-when-no-ice-in-hand
    ;; Bogus prompt when no ice in hand
    (do-game
     (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                       :deck [(qty "Hedge Fund" 5)]}})
     (take-credits state :corp)
     (run-empty-server state "HQ")
     (is (prompt-is-type? state :runner :waiting) "Runner waiting on Mti ability")
     (click-prompt state :corp "Carry on!")))

(deftest issuaq-adaptics-sustaining-diversity
  ;; Issuaq Adaptics: Sustaining Diversity
  (do-game
    (new-game {:corp {:id "Issuaq Adaptics: Sustaining Diversity"
                      :hand ["Merger" (qty "Blood in the Water" 2)]
                      :deck [(qty "Hedge Fund" 5)]}
               :runner {:hand []}})
    (let [issuaq (get-in @state [:corp :identity])]
      (play-from-hand state :corp "Blood in the Water" "New remote")
      (play-from-hand state :corp "Merger" "New remote")
      (play-from-hand state :corp "Blood in the Water" "New remote")
      (is (changed? [(get-counters (refresh issuaq) :power) 0]
            (score state :corp (get-content state :remote1 0)))
          "No power counter added to ID when agenda is scored on the same turn it's installed")
      (take-credits state :corp)
      (take-credits state :runner)
      (advance state (get-content state :remote2 0) 3)
      (is (changed? [(get-counters (refresh issuaq) :power) 0]
            (score state :corp (get-content state :remote2 0)))
          "No power counter added to ID when agenda is scored on the same turn it's advanced")
      (is (changed? [(get-counters (refresh issuaq) :power) 1]
            (score state :corp (get-content state :remote3 0)))
          "1 power counter added to ID")
      (is (= :corp (:winner @state))))))

(deftest issuaq-adaptics-sustaining-diversity-with-employee-strike
  ;; Issuaq Adaptics: Sustaining Diversity - ability goes back after Employee Strike is trashed
  (do-game
    (new-game {:corp {:id "Issuaq Adaptics: Sustaining Diversity"
                      :hand ["Merger" (qty "Blood in the Water" 2)]
                      :deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Employee Strike"]}})
    (let [issuaq (get-in @state [:corp :identity])]
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Merger" "New remote")
      (advance state (get-content state :remote1 0) 3)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (changed? [(get-counters (refresh issuaq) :power) 1]
            (score state :corp (get-content state :remote1 0)))
          "1 power counter added to ID")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (play-and-score state "Blood in the Water")
      (play-and-score state "Blood in the Water")
      (is (= :corp (:winner @state))))))

(deftest nasir-meidan-cyber-explorer
  ;; Nasir
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [iwall (get-ice state :hq 0)]
        (is (= 5 (:credit (get-runner))))
        (rez state :corp iwall)
        (run-continue state)
        (is (= 1 (:credit (get-runner))) "Credits at 1 after Nasir ability trigger"))))

(deftest nasir-meidan-cyber-explorer-with-xanadu
    ;; with Xanadu
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"
                          :deck ["Xanadu"]
                          :credits 6}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Xanadu")
      (run-on state "HQ")
      (let [iwall (get-ice state :hq 0)]
        (is (= 3 (:credit (get-runner))) "Pay 3 to install Xanadu")
        (rez state :corp iwall)
        (run-continue state)
        (is (= 2 (:credit (get-runner))) "Gain 1 more credit due to Xanadu"))))

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

(deftest nbn-controlling-the-message-trace-to-tag-runner-when-first-installed-corp-card-is-trashed-issue-2321
    ;; Trace to tag Runner when first installed Corp card is trashed. Issue #2321
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
      (is (no-prompt? state :runner) "Forger can't avoid the tag")
      (is (= 1 (count-tags state)) "Runner took 1 unpreventable tag")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (no-prompt? state :corp) "No trace chance on 2nd trashed card of turn")))

(deftest nbn-controlling-the-message-interaction-with-dedicated-response-team
    ;; Interaction with Dedicated Response Team
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck ["Launch Campaign" "Dedicated Response Team"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner took 1 unpreventable tag")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from DRT")))

(deftest nbn-controlling-the-message-trace-shouldn-t-fire-on-second-trash-after-trash-during-direct-access-run-4168
    ;; Trace shouldn't fire on second trash after trash during Direct Access run. #4168
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
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :runner "Yes")
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (no-prompt? state :corp) "CtM shouldn't fire")
      (is (no-prompt? state :runner) "Runner shouldn't have prompt")
      (is (zero? (count-tags state)) "Runner took 1 unpreventable tag")))

(deftest nbn-controlling-the-message-trace-should-fire-on-first-trash-of-a-corp-card-after-a-runner-card-is-trashed
    ;; Trace should fire on first trash of a Corp card after a Runner card is trashed
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
      (end-phase-12 state :runner)
      (is (zero? (count (get-resource state))))
      (is (= 1 (count (:hand (get-runner)))))
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (not (no-prompt? state :corp)) "Corp should have a Trace prompt")
      (click-prompt state :corp "No")))

(deftest nbn-making-news-pay-credits-and-not-refilling-on-disabled-issue-2439
    ;; Pay credits, and not refilling on disabled. Issue #2439
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
        (is (= 2 (get-counters (refresh nbn-mn) :recurring)) "Recurring credits refill once MN isn't disabled anymore"))))

(deftest nbn-reality-plus
  ;; NBN: Reality Plus
  (do-game
     (new-game {:corp {:id "NBN: Reality Plus"
                       :credits 40
                       :hand [(qty "Data Raven" 3)]}})
     (play-from-hand state :corp "Data Raven" "HQ")
     (play-from-hand state :corp "Data Raven" "HQ")
     (take-credits state :corp)
     (let [dr (get-ice state :hq 0)
           dr2 (get-ice state :hq 1)]
       (run-on state :hq)
       (rez state :corp (refresh dr2))
       (run-continue state)
       (click-prompt state :runner "Take 1 tag")
       (is (changed? [(:credit (get-corp)) 2]
             (click-prompt state :corp "Gain 2 [Credits]"))
           "Gain 2 credit from NBN: Reality Plus")
       (run-continue-until state :approach-ice)
       (rez state :corp (refresh dr))
       (run-continue state)
       (click-prompt state :runner "Take 1 tag")
       (is (no-prompt? state :corp) "No prompt for the Corp for second tag")))
  (do-game
     (new-game {:corp {:id "NBN: Reality Plus"
                       :credits 40
                       :deck [(qty "Hedge Fund" 10)]
                       :hand [(qty "Data Raven" 3)]}})
     (play-from-hand state :corp "Data Raven" "HQ")
     (play-from-hand state :corp "Data Raven" "HQ")
     (take-credits state :corp)
     (let [dr (get-ice state :hq 0)
           dr2 (get-ice state :hq 1)]
       (run-on state :hq)
       (rez state :corp (refresh dr2))
       (run-continue state)
       (click-prompt state :runner "Take 1 tag")
       (is (changed? [(count (:hand (get-corp))) 2]
             (click-prompt state :corp "Draw 2 cards"))
           "Draw 2 cards from NBN: Reality Plus")
       (run-continue-until state :approach-ice)
       (rez state :corp (refresh dr))
       (run-continue state)
       (click-prompt state :runner "Take 1 tag")
       (is (no-prompt? state :corp) "No prompt for the Corp for second tag"))))

(deftest near-earth-hub
  ;; NEH - draws a card when you install a card in a new remote
  (do-game
   (new-game {:corp {:id "Near-Earth Hub: Broadcast Center"
                     :hand [(qty "Advanced Assembly Lines" 5) "PAD Campaign"]
                     :deck [(qty "Advanced Assembly Lines" 5)]}})
   (is (changed? [(count (:hand (get-corp))) 0]
         (play-from-hand state :corp "Advanced Assembly Lines" "New remote"))
       "Draw 1 card (net 0) with NEH")
   (is (changed? [(count (:hand (get-corp))) -1]
         (play-from-hand state :corp "Advanced Assembly Lines" "New remote"))
       "NEH does not fire twice")
   (take-credits state :corp)
   (rez state :corp (get-content state :remote1 0))
   (is (changed? [(count (:hand (get-corp))) 0]
         (card-ability state :corp (get-content state :remote1 0) 0)
         (click-card state :corp "PAD Campaign")
         (click-prompt state :corp "New remote"))
       "NEH on runner turn draws 1")))

(deftest near-earth-hub-install-from-rnd
  ;; NEH does not fail when installing from R&D
  (do-game
   (new-game {:corp {:id "Near-Earth Hub: Broadcast Center"
                     :hand ["Architect" "Ballista" "Chum" "Drafter"
                            "Eli 1.0" "Fenris" "Galahad"]}})
   ;; just starting them in deck has them unordered - need to to it the hard way
   (core/move state :corp (find-card "Ballista" (:hand (get-corp))) :deck)
   (core/move state :corp (find-card "Chum" (:hand (get-corp))) :deck)
   (core/move state :corp (find-card "Drafter" (:hand (get-corp))) :deck)
   (core/move state :corp (find-card "Eli 1.0" (:hand (get-corp))) :deck)
   (core/move state :corp (find-card "Fenris" (:hand (get-corp))) :deck)
   (core/move state :corp (find-card "Galahad" (:hand (get-corp))) :deck)
   (is (= ["Ballista" "Chum" "Drafter" "Eli 1.0" "Fenris" "Galahad"]
          (map :title (:deck (get-corp)))) "DECK is BCDEFG")
   (play-from-hand state :corp "Architect" "HQ")
   (rez state :corp (get-ice state :hq 0))
   (take-credits state :corp)
   (run-on state :hq)
   (run-continue state)
   (fire-subs state (get-ice state :hq 0))
   (is (changed? [(count (:hand (get-corp))) 1]
         (click-prompt state :corp "OK")
         (click-prompt state :corp "Ballista")
         (click-prompt state :corp "New remote"))
       "drew 1 card with neh")
   (is (= ["Drafter" "Eli 1.0" "Fenris" "Galahad"]
          (map :title (:deck (get-corp)))) "Deck is DEFG")
   (is (= ["Chum"]
          (map :title (:hand (get-corp)))) "Hand is chummy")
   (is (changed? [(count (:hand (get-corp))) -1]
         (click-card state :corp "Chum")
         (click-prompt state :corp "New remote"))
       "installed 1 card with architect")
   (is (= "Chum" (:title (get-ice state :remote2 0))))
   (is (= "Ballista" (:title (get-ice state :remote1 0))))))

(deftest nero-severn-information-broker
  ;; Nero Severn: Information Broker
  (do-game
      (new-game {:runner {:id "Nero Severn: Information Broker"
                          :deck [(qty "Sure Gamble" 10)]}
                :corp { :deck ["Komainu"]}})
      (play-from-hand state :corp "Komainu" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Nero ability")
      (is (= "Jack out?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (run-on state "HQ")
      (run-continue state)
      (is (not (= "Do you want to jack out?" (:msg (prompt-map :runner))))
          "No prompt for Nero ability because we used it on previous run")))

(deftest nero-severn-information-broker-receives-prompt-on-second-run-if-ability-not-used
    ;; Receives prompt on second run, if ability not used
    (do-game
      (new-game {:runner {:id "Nero Severn: Information Broker"
                          :deck [(qty "Sure Gamble" 10)]}
                :corp { :deck ["Guard"]}})
      (play-from-hand state :corp "Guard" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Nero ability")
      (is (= "Jack out?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "No")
      (fire-subs state (get-ice state :hq 0))
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be again waiting on Runner for Nero ability")
      (is (= "Jack out?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")))

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
  ;; Next Design.  Install up to 3 pieces of ice before game starts, one per server max, and re-draw to 5
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
      (rez state :corp s2)
      (run-continue state)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s2 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game")
      (run-continue-until state :approach-ice)
      (rez state :corp s1)
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
      (click-card state :corp (first (:hand (get-corp))))
      (play-from-hand state :runner "Datasucker")
      (is (= 2 (count (:discard (get-corp)))) "Playing virus should cause card to be trashed from R&D")
      (is (= 4 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
      (play-from-hand state :runner "Sure Gamble")
      (is (= 2 (count (:discard (get-corp)))) "Playing non-virus should not cause card to be trashed from R&D")
      (click-draw state :runner)
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
    (is (= 3 (count (:discard (get-corp)))) "Playing virus via Clone Chip on corp's turn should trigger Noise ability")
      (is (= 2 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
      ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
      (let [chip-2 (get-hardware state 0)]
        (card-ability state :runner chip-2 0)
        (click-card state :runner (find-card "Sharpshooter" (:discard (get-runner))))
        (let [ss (get-program state 2)]
          (is (not (nil? ss)))
          (is (= (:title ss) "Sharpshooter"))))
    (is (= 3 (count (:discard (get-corp)))) "Playing non-virus via Clone Chip on corp's turn should not trigger Noise ability")))

(deftest noise-hacker-extraordinaire-noise-ar-enhanced-security-issue-5345
    ;; Noise + AR-Enhanced Security. Issue #5345
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)]
                        :hand ["AR-Enhanced Security"]}
                 :runner {:id "Noise: Hacker Extraordinaire"
                          :deck ["Datasucker"]}})
      (play-and-score state "AR-Enhanced Security")
      (take-credits state :corp)
      (play-from-hand state :runner "Datasucker")
      (is (zero? (count-tags state)) "Runner took no tag for milling")))

(deftest nuvem-sa
  (do-game
    (new-game {:corp {:id "Nuvem SA: Law of the Land"
                      :deck [(qty "Hedge Fund" 20)]
                      :hand [(qty "Hedge Fund" 2) "Ice Wall" "Tree Line" "Hasty Relocation"]}})
    (play-from-hand state :corp "Hedge Fund")
    (is (= "The top card of R&D is: Hedge Fund" (:msg (get-prompt state :corp))))
    (click-prompt state :corp "OK")
    (is (changed? [(:credit (get-corp)) 2
                   (count (:deck (get-corp))) -1
                   (count (:discard (get-corp))) 1]
          (click-prompt state :corp "Yes"))
        "Corp gains 2 credits to trash top of R&D")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (expend state :corp (find-card "Tree Line" (:hand (get-corp))))
    (click-card state :corp (get-ice state :hq 0))
    (is (= "The top card of R&D is: Hedge Fund" (:msg (get-prompt state :corp))))
    (click-prompt state :corp "OK")
    (is (changed? [(:credit (get-corp)) 0
                   (count (:deck (get-corp))) -1
                   (count (:discard (get-corp))) 1]
          (click-prompt state :corp "Yes"))
        "Corp doesn't gain credits after trashing second card from R&D")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Hedge Fund")
    (is (= "The top card of R&D is: Hedge Fund" (:msg (get-prompt state :corp))))
    (click-prompt state :corp "OK")
    (is (changed? [(:credit (get-corp)) 0
                   (count (:deck (get-corp))) 0
                   (count (:discard (get-corp))) 0]
          (click-prompt state :corp "No"))
        "Corp can choose to not trash top of R&D")
    (is (changed? [(:credit (get-corp)) 2]
          (play-from-hand state :corp "Hasty Relocation"))
        "Corp gains 2 credits from R&D trash even when not from ID trash ability")))

(deftest nuvem-sa-only-trigger-on-corp-turn
  (do-game
    (new-game {:corp   {:id "Nuvem SA: Law of the Land"
                        :deck [(qty "Hedge Fund" 20)]
                        :hand ["Hedge Fund"]}
               :runner {:hand ["Cookbook" "Gravedigger"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cookbook")
    (play-from-hand state :runner "Gravedigger")
    (click-prompt state :runner "Yes")
    (is (changed? [(:credit (get-corp)) 0]
          (card-ability state :runner (get-program state 0) 0))
        "Nuvem should not fire on Runner's turn")))

(deftest null-whistleblower
  ;; Null
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
        (is (no-prompt? state :runner) "Ability won't work on unrezzed ice")
        (rez state :corp wrap2)
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 5 (get-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
        (run-continue-until state :approach-ice)
        (rez state :corp wrap1)
        (run-continue state)
        (is (no-prompt? state :runner) "Ability already used this turn")
        (run-continue state :movement)
        (run-jack-out state)
        (is (= 7 (get-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))

(deftest null-whistleblower-does-not-affect-next-ice-when-current-is-trashed-issue-1788
    ;; does not affect next ice when current is trashed. Issue #1788.
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
        (rez state :corp spider)
        (rez state :corp iw)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner (refresh spider))
        (run-on state "HQ")
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner (first (:hand (get-runner))))
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Null")
        (is (= 1 (get-strength (refresh iw))) "Ice Wall not reduced by Null"))))

(deftest null-whistleblower-receives-prompt-on-second-run-if-ability-not-used
    ;; Receives prompt on second run, if ability not used
    (do-game
      (new-game {:runner {:id "Null: Whistleblower"
                          :deck [(qty "Sure Gamble" 10)]}
                :corp { :deck ["Guard"]}})
      (play-from-hand state :corp "Guard" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Null ability")
      (is (not (no-prompt? state :runner)) "Null: Whistleblower prompt")
      (click-prompt state :runner "No")
      (fire-subs state (get-ice state :hq 0))
      (run-on state "HQ")
      (run-continue state)
      (is (prompt-is-type? state :corp :waiting) "Corp should now be again waiting on Runner for Null ability")
      (is (not (no-prompt? state :runner)) "Null: Whistleblower prompt")
      (click-prompt state :runner "Yes")))

(deftest nyusha-sable-sintashta
  ;; Nyusha "Sable" Sintashta start of turn: mark server. First successful run on mark: gain click
  (do-game
    (new-game {:runner {:id "Nyusha \"Sable\" Sintashta: Symphonic Prodigy"}
               :corp {:hand ["Hedge Fund"] :deck ["Hedge Fund"] :discard ["Hedge Fund"]}})
    (take-credits state :corp)
    (run-on state (zone->name (:mark @state)))
    (is (changed? [(:click (get-runner)) 1]
          (run-continue state))
        "gained 1 click from running the mark")))

(deftest nyusha-sable-sintashta-with-virtuoso
  ;; Multiple cards setting a mark are idempotent
  (do-game
    (new-game {:runner {:id "Nyusha \"Sable\" Sintashta: Symphonic Prodigy" :hand ["Virtuoso"]}
               :corp {:deck [(qty "Hedge Fund" 5)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Virtuoso")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [virt (get-hardware state 0)
          sable (get-in @state [:runner :identity])]
      (is (= true (is-mark? state (unknown->kw (:card-target virt)))))
      (is (= true (is-mark? state (unknown->kw (:card-target sable)))))
      (is (last-log-contains? state "identifies their mark"))
      (is (not (second-last-log-contains? state "identifies their mark"))))))

(deftest ob-superheavy-logistics-basic-test
  ;; The ability works, and it works once per turn - depends on Extract to be correct
  (do-game
   (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                     :hand [(qty "Extract" 3) (qty "Launch Campaign" 2) "PAD Campaign"]
                     :deck [(qty "Prisec" 2) "Anoetic Void" "Ice Wall"]
                     :credits 10}})
   (core/gain state :corp :click 10)
   ;; launch campaign is 1 cost, and can summon prisec
   (play-from-hand state :corp "Launch Campaign" "New remote")
   (play-from-hand state :corp "Launch Campaign" "New remote")
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (rez state :corp (get-content state :remote1 0))
   (rez state :corp (get-content state :remote2 0))
   (rez state :corp (get-content state :remote3 0))
   ;; decline to use
   (play-from-hand state :corp "Extract")
   (click-card state :corp (get-content state :remote1 0))
   (click-prompt state :corp "No")
   (is (no-prompt? state :corp) "No further prompt for Ob")
   ;; pull prisec
   (play-from-hand state :corp "Extract")
   (click-card state :corp (get-content state :remote2 0))
   (click-prompt state :corp "Yes")
   (click-prompt state :corp "Prisec")
   (click-prompt state :corp "New remote")
   (is (= "Prisec" (:title (get-content state :remote4 0))) "Installed Prisec in remote")
   (is (rezzed? (get-content state :remote4 0)) "Prisec is rezzed")
   ;; ability can't be used again
   (play-from-hand state :corp "Extract")
   (click-card state :corp (get-content state :remote3 0))
   (is (no-prompt? state :corp) "No prompt to use Ob again")))

(deftest ob-superheavy-logistics-additional-costs
  ;; doesn't waive additional costs to rez (ie corp. town)
  (do-game
   ;; can't pay cost
   (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                     :hand ["Extract" "PAD Campaign"]
                     :deck ["Corporate Town"]
                     :credits 10}})
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (rez state :corp (get-content state :remote1 0))
   (play-from-hand state :corp "Extract")
   (click-card state :corp "PAD Campaign")
   (click-prompt state :corp "Yes")
   (click-prompt state :corp "Corporate Town")
   (click-prompt state :corp "New remote")
   (is (no-prompt? state :corp) "No prompt to rez")
   (is (= "Corporate Town" (:title (get-content state :remote2 0))) "Installed C. Town in remote")
   (is (not (rezzed? (get-content state :remote2 0))) "Did not rez C. Town"))
  (do-game
   ;; refuse to pay cost
   (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                     :hand ["Extract" "PAD Campaign" "Hostile Takeover"]
                     :deck ["Corporate Town"]
                     :credits 10}})
   (core/gain state :corp :click 10)
   (play-and-score state "Hostile Takeover")
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (rez state :corp (get-content state :remote2 0))
   (play-from-hand state :corp "Extract")
   (click-card state :corp "PAD Campaign")
   (click-prompt state :corp "Yes")
   (click-prompt state :corp "Corporate Town")
   (click-prompt state :corp "No")
   (click-prompt state :corp "New remote")
   (is (no-prompt? state :corp) "No prompt to rez")
   (is (= "Corporate Town" (:title (get-content state :remote3 0))) "Installed C. Town in remote")
   (is (not (rezzed? (get-content state :remote3 0))) "Did not rez C. Town"))
  (do-game
   ;; pay additional cost to rez
   (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                     :hand ["Extract" "PAD Campaign" "Hostile Takeover"]
                     :deck ["Corporate Town"]
                     :credits 10}})
   (core/gain state :corp :click 10)
   (play-and-score state "Hostile Takeover")
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (rez state :corp (get-content state :remote2 0))
   (play-from-hand state :corp "Extract")
   (click-card state :corp "PAD Campaign")
   (click-prompt state :corp "Yes")
   (click-prompt state :corp "Corporate Town")
   (click-prompt state :corp "Yes")
   (click-prompt state :corp "New remote")
   (click-card state :corp "Hostile Takeover")
   (is (no-prompt? state :corp) "No prompt to rez")
   (is (= "Corporate Town" (:title (get-content state :remote3 0))) "Installed C. Town in remote")
   (is (rezzed? (get-content state :remote3 0)) "rezzed C. Town")))

(deftest ob-superheavy-logistics-additional-rez-costs
  ;; should waive additional credit costs to rez (ie tread lightly)
  (do-game
    (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                      :hand ["Mavirus" "Ice Wall"]
                      :deck [(qty "Ping" 5) ]
                      :credits 10}
               :runner {:hand ["Tread Lightly"]}})
    (play-from-hand state :corp "Mavirus" "New remote")
    (play-from-hand state :corp "Ice Wall" "Server 1")
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Tread Lightly")
    (click-prompt state :runner "Server 1")
    (card-ability state :corp (get-content state :remote1 0) 0)
    (is (changed? [(:credit (get-corp)) 0]
          (click-prompt state :corp "Yes")
          (click-prompt state :corp "Ping")
          (click-prompt state :corp "Server 1"))
        "Ob should not pay extra credit costs")))

(deftest ob-superheavy-logistics-fail-to-find
  ;; If no cards in R&D match the search cost, ability can be declined
  (do-game
   (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                     :hand ["Extract" "PAD Campaign"]
                     :deck ["Anoetic Void"]}})
   (play-from-hand state :corp "PAD Campaign" "New remote")
   (rez state :corp (get-content state :remote1 0))
   (play-from-hand state :corp "Extract")
   (click-card state :corp (get-content state :remote1 0))
   (click-prompt state :corp "Yes")
   (is (= ["Done"] (prompt-buttons :corp)) "Sole option available is Done")
   (click-prompt state :corp "Done")))

(deftest ob-superheavy-logistics-public-agendas
  ;; If no cards in R&D match the search cost, ability can be declined
  (do-game
   (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                     :hand ["Oaktown Renovation" "Extract"]
                     :deck ["Anoetic Void"]}})
   (play-from-hand state :corp "Oaktown Renovation" "New remote")
   (play-from-hand state :corp "Extract")
   (click-card state :corp "Oaktown Renovation")
   (is (no-prompt? state :corp) "No Ob prompt")
   (is (find-card "Oaktown Renovation" (:discard (get-corp))) "Oaktown is trashed")))

(deftest ob-superheavy-logistics-shuffle-with-rashida
  ;; you can search for -1 cost cards as an excuse to shuffle your deck
  (do-game
    (new-game {:corp {:id "Ob Superheavy Logistics: Extract. Export. Excel."
                      :hand ["Rashida Jaheem"]
                      :deck [(qty "Hedge Fund" 15)]}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (take-credits state :corp)
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :runner)
    (is (:corp-phase-12 @state) "Corp has opportunity to use Rashida")
    (card-ability state :corp (get-content state :remote1 0) 0)
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Yes")
    (is (last-log-contains? state "shuffle") "Ob superheavy should shuffle R&D")))

(deftest omar-keung-conspiracy-theorist-make-a-successful-run-on-the-chosen-server-once-per-turn
    ;; Make a successful run on the chosen server once per turn
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])]
        (card-ability state :runner omar 0)
        (run-continue state)
        (click-prompt state :runner "HQ")
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (accessing state "Hedge Fund"))
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
        (click-prompt state :runner "HQ")
        (is (= [:hq :rd] (get-in @state [:runner :register :successful-run]))))))

(deftest omar-keung-conspiracy-theorist-ash-prevents-access-but-not-successful-run
    ;; Ash prevents access, but not successful run
    (do-game
      (new-game {:corp {:deck ["Ash 2X3ZB9CY"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            ash (get-content state :hq 0)]
        (rez state :corp ash)
        (card-ability state :runner omar 0)
        (run-continue state)
        (click-prompt state :runner "HQ")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= (:cid ash) (-> (prompt-map :runner) :card :cid)))
        (is (= :hq (-> (get-runner) :register :successful-run first))))))

(deftest omar-keung-conspiracy-theorist-crisium-grid-prevents-prompt
    ;; Crisium Grid prevents prompt
    (do-game
      (new-game {:corp {:deck ["Crisium Grid"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            cr (get-content state :archives 0)]
        (rez state :corp cr)
        (card-ability state :runner omar 0)
        (run-continue state)
        (is (= (:cid cr) (-> (prompt-map :runner) :card :cid)))
        (is (empty? (-> (get-runner) :register :successful-run)))
        (is (= :archives (get-in @state [:run :server 0]))))))

(deftest omar-keung-conspiracy-theorist-when-selecting-r-d-ability-adds-counters-to-medium
    ;; When selecting R&D, ability adds counters to Medium
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck ["Medium"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Medium")
      (let [omar (get-in @state [:runner :identity])
            medium (get-program state 0)]
        (card-ability state :runner omar 0)
        (run-continue state)
        (click-prompt state :runner "R&D")
        (is (= 1 (get-counters (refresh medium) :virus))))))

(deftest omar-keung-conspiracy-theorist-when-selecting-hq-ability-adds-counters-to-nerve-agent
    ;; When selecting HQ, ability adds counters to Nerve Agent
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck ["Nerve Agent"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Nerve Agent")
      (let [omar (get-in @state [:runner :identity])
            nerve (get-program state 0)]
        (card-ability state :runner omar 0)
        (run-continue state)
        (click-prompt state :runner "HQ")
        (is (= 1 (get-counters (refresh nerve) :virus))))))

(deftest omar-keung-conspiracy-theorist-moving-to-a-different-server-shouldn-t-trigger-ability-issue-3969
    ;; Moving to a different server shouldn't trigger ability. Issue #3969
    (do-game
      (new-game {:corp {:hand ["Bullfrog"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"}})
      (play-from-hand state :corp "Bullfrog" "Archives")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            frog (get-ice state :archives 0)]
        (card-ability state :runner omar 0)
        (rez state :corp frog)
        (run-continue state)
        (is (= [:archives] (:server (get-run))))
        (card-subroutine state :corp frog 0)
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "1 [Credits]")
        (click-prompt state :corp "R&D")
        (is (= [:rd] (:server (get-run))))
        (run-continue state)
        (is (no-prompt? state :corp))
        (is (no-prompt? state :runner)))))

(deftest pravdivost-consulting-fake-prompt
  ;; Pravdivost Consulting: Political Solutions
  (do-game
    (new-game {:corp {:id "Pravdivost Consulting: Political Solutions"
                      :hand ["PAD Campaign"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (run-on state :hq)
    (run-continue state)
    (is (not (no-prompt? state :runner)) "Fake prompt is displayed to the Runner")
    (click-prompt state :corp "Done")))

(deftest pravdivost-consulting-happy-path
  (do-game
    (new-game {:corp {:id "Pravdivost Consulting: Political Solutions"
                      :hand ["NGO Front"]}})
    (play-from-hand state :corp "NGO Front" "New remote")
    (take-credits state :corp)
    (run-on state :hq)
    (run-continue state)
    (click-card state :corp "NGO Front")
    (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "NGO was advanced")))

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
      (rez state :corp iwall)
      (run-continue state)
      (card-ability state :runner q 0)
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (run-continue state :movement)
      (run-jack-out state)
      (click-credit state :runner)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (refresh q) 0)
      (is (not (last-log-contains? state qmsg)) "Quetzal ability did not trigger")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (click-credit state :runner)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (refresh q) 0)
      (click-prompt state :runner "End the run")
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (core/jack-out state :runner nil))))

(deftest quetzal-free-spirit-quetzal-cannot-break-more-than-1-barrier-sub-during-the-same-encounter-5944
    ;; Quetzal cannot break more than 1 Barrier sub during the same encounter #5944
    (do-game
      (new-game {:corp {:deck [(qty "Spiderweb" 3)]}
               :runner {:id "Quetzal: Free Spirit"
                        :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [q (get-in @state [:runner :identity])
            spiderweb (get-ice state :hq 0)]
        (rez state :corp spiderweb)
        (run-continue state)
        (card-ability state :runner q 0)
        (click-prompt state :runner "End the run")
        (is (no-prompt? state :runner) "No prompt for further breaking"))))

(deftest reina-roja-freedom-fighter
  ;; Reina Roja - Increase cost of first rezzed piece of ice
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 3)]}
               :runner {:id "Reina Roja: Freedom Fighter"}})
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (rez state :corp quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))

(deftest reina-roja-freedom-fighter-blue-sun-doesn-t-consider-reina-s-ability-5192
    ;; Blue Sun doesn't consider Reina's ability #5192
    (do-game
      (new-game {:corp {:id "Blue Sun: Powering the Future"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:id "Reina Roja: Freedom Fighter"}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (is (changed? [(:credit (get-corp)) 1]
            (click-card state :corp "Ice Wall"))
          "Gain 1 credit from Blue Sun")))

(deftest rene-loup-arcemont-party-animal
  ;; "René \"Loup\" Arcemont: Party Animal"
  (do-game
   (new-game {:corp {:hand [(qty "NGO Front" 2)]}
              :runner {:id "René \"Loup\" Arcemont: Party Animal"
                       :hand ["Sure Gamble"]
                       :deck [(qty "Sure Gamble" 3)]}})
   (play-from-hand state :corp "NGO Front" "New remote")
   (play-from-hand state :corp "NGO Front" "New remote")
   (take-credits state :corp)
   (run-empty-server state "Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Runner starts with 1 card")
   (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
   (click-prompt state :runner "Pay 1 [Credits] to trash")
   (is (= 5 (:credit (get-runner))) "Gain 1 credit from trashing accessed card (and paid 1 to trash)")
   (is (= 2 (count (:hand (get-runner)))) "Runner draws 1 card")
   (run-empty-server state "Server 2")
   (click-prompt state :runner "Pay 1 [Credits] to trash")
   (is (= 4 (:credit (get-runner))) "Runner doesn't gain 1 credit from trashing accessed card")
   (is (= 2 (count (:hand (get-runner)))) "Runner doesn't draw 1 card")))

(deftest rielle-kit-peddler-transhuman
  ;; Rielle "Kit" Peddler - Give ice Code Gate
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
               :runner {:id "Rielle \"Kit\" Peddler: Transhuman"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [iwall (get-ice state :hq 0)]
      (rez state :corp iwall)
      (run-continue state)
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
      (rez state :corp (refresh splicer) {:expect-rez false})
      (is (not (rezzed? (refresh splicer))) "Gene Splicer did not rez")
      (take-credits state :corp)
      (rez state :corp (refresh splicer) {:expect-rez false})
      (is (not (rezzed? (refresh splicer))) "Gene Splicer did not rez on the Runner's turn")
      (take-credits state :runner)
      (rez state :corp (refresh splicer))
      (is (rezzed? (refresh splicer)) "Gene Splicer now rezzed")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "House of Knives" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [house (get-content state :remote2 0)]
        (advance state house)
        (advance state house)
        (score state :corp (refresh house))
        (is (empty? (:scored (get-corp))) "House of Knives not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (score state :corp (refresh house))
        (is (= 1 (:agenda-point (get-corp))) "House of Knives was able to be scored")))))

(deftest sebastiao-pessoa
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 3)]}
               :runner {:id "Sebastião Souza Pessoa: Activist Organizer"
                        :hand ["Verbal Plasticity" "Professional Contacts" "Smartware Distributor"]}})
    (gain-tags state :runner 1)
    (is (changed? [(:credit (get-runner)) 0]
                  (click-card state :runner (find-card "Verbal Plasticity" (:hand (get-runner)))))
        "Verbal Plasticity is not a Connection")
    (is (changed? [(:credit (get-runner)) -3]
                  (click-card state :runner (find-card "Professional Contacts" (:hand (get-runner)))))
        "Install Professional Contacts paying 2 less")
    (is (= "Professional Contacts" (:title (get-resource state 0))) "Professional Contacts is installed")
    (gain-tags state :runner 1)
    (is (no-prompt? state :runner) "No prompt when Seb already has tags")
    (is (changed? [(count (:hand (get-corp))) -1]
                  (trash-resource state)
                  (click-card state :corp (get-resource state 0))
                  (click-prompt state :corp "Trash 1 card from your hand")
                  (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp)))))
        "The corp has to trash a card from HQ to trash a resource")))

(deftest seidr-laboratories-destiny-defined
  ;; Seidr Laboratories: Destiny Defined
  (do-game
      (new-game {:corp {:id "Seidr Laboratories: Destiny Defined"
                        :discard ["IPO"]
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (is (prompt-map :corp))
      (is (= "Choose a card to add to the top of R&D" (:msg (prompt-map :corp))))
      (click-card state :corp "IPO")
      (is (find-card "IPO" (:deck (get-corp))))))

(deftest seidr-laboratories-destiny-defined-must-be-used-during-a-run
    ;; Must be used during a run
    (do-game
      (new-game {:corp {:id "Seidr Laboratories: Destiny Defined"
                        :discard ["Hedge Fund"]}})
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (is (no-prompt? state :corp))
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (is (prompt-map :corp))))

(deftest seidr-laboratories-destiny-defined-must-have-at-least-1-card-in-archives
    ;; Must have at least 1 card in archives
    (do-game
      (new-game {:corp {:id "Seidr Laboratories: Destiny Defined"
                        :hand ["Hedge Fund"]}})
      (take-credits state :corp)
      (run-on state :hq)
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (is (no-prompt? state :corp))))

(deftest silhouette-stealth-operative-expose-trigger-ability-resolves-completely-before-access-issue-2173
    ;; Expose trigger ability resolves completely before access. Issue #2173
    (do-game
      (new-game {:corp {:hand ["Psychic Field" "Fetal AI"]
                        :deck [(qty "Hedge Fund" 10)]}
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

(deftest silhouette-stealth-operative-with-temujin-broken-interaction-with-other-successful-run-triggers-issue-1968
    ;; with Temüjin; broken interaction with other successful-run triggers. Issue #1968
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
      (is (= "HQ" (:card-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
      (is (= 8 (:credit (get-runner))) "Gained 4cr")
      ;; second run
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= "HQ" (:card-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 12 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin")))

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
    (is (accessing state "Quandary"))
    (click-prompt state :runner "No action")
    (is (accessing state "Quandary"))
    (click-prompt state :runner "No action")
    (is (accessing state "Quandary"))
    (click-prompt state :runner "No action")
    (is (not (:run @state)))
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (click-prompt state :corp (find-card "The Maker's Eye" (:discard (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rfg]))) "One card RFGed")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (no-prompt? state :corp) "Cannot use Skorpios twice")))

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
      (rez state :corp lc1)
      (is (= 4 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (rez state :corp lc3)
      (is (= 4 (:credit (get-runner)))
          "Runner did not lose credit from second Spark rez")
      (take-credits state :corp)
      (run-on state "Server 1")
      (rez state :corp lc2)
      (is (= 3 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Runner turn)"))))

(deftest sportsmetal-go-big-or-go-home-gain-2-credits-on-score
    ;; Gain 2 credits on score
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 7 (:credit (get-corp))) "Corp gains 2 credits")))

(deftest sportsmetal-go-big-or-go-home-gain-2-credits-on-steal
    ;; Gain 2 credits on steal
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "Gain 2 [Credits]")
      (is (= 9 (:credit (get-corp))) "Corp gains 2 credits")))

(deftest sportsmetal-go-big-or-go-home-draw-2-cards-on-score
    ;; Draw 2 cards on score
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger" (qty "Hedge Fund" 2)]}})
      (starting-hand state :corp ["Merger"])
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (empty? (:hand (get-corp))) "Corp starts with no cards")
      (click-prompt state :corp "Draw 2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards")))

(deftest sportsmetal-go-big-or-go-home-draw-2-cards-on-steal
    ;; Draw 2 cards on steal
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
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards")))

(deftest sso-industries-fueling-innovation
  ;; SSO Industries: Fueling Innovation - add advancement tokens on ice for faceup agendas
  (do-game
    (new-game {:corp {:id "SSO Industries: Fueling Innovation"
                      :deck [(qty "Hortum" 2) (qty "Oaktown Renovation" 2) "Braintrust"]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (is (no-prompt? state :corp) "Not prompted when no faceup agenda available")
    (take-credits state :runner)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (take-credits state :corp)
    (is (no-prompt? state :corp) "Not prompted when no ice available")
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
      (is (no-prompt? state :corp) "Not prompted when all ice advanced"))))

(deftest steve-cambridge-master-grifter-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Hedge Fund"]}
                 :runner {:id      "Steve Cambridge: Master Grifter"
                          :discard ["Sure Gamble" "Easy Mark"]}})
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "Yes")
      (click-card state :runner "Sure Gamble")
      (click-card state :runner "Easy Mark")
      (click-prompt state :corp "Sure Gamble")
      (click-prompt state :runner "No action")
      (is (= "Easy Mark" (-> (get-runner) :hand first :title)) "Easy Mark should be in the hand")
      (is (= "Sure Gamble" (-> (get-runner) :rfg first :title)) "Sure Gamble should be removed from game")))

(deftest steve-cambridge-master-grifter-heap-locked-test
    ;; Heap Locked Test
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5) "Blacklist"]}
                 :runner {:id      "Steve Cambridge: Master Grifter"
                          :discard ["Sure Gamble" "Easy Mark"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (run-empty-server state :hq)
      (is (accessing state "Hedge Fund") "No Steve Cambridge prompt, go direct to access.")))

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
      (rez state :corp i1)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (not (no-prompt? state :corp)) "Corp prompted to trigger Strategic Innovations")
      (click-card state :corp (first (:discard (get-corp))))
      (is (empty? (:discard (get-corp))) "Hedge Fund moved back to R&D")
      (take-credits state :corp)
      (rez state :corp i2)
      (take-credits state :runner)
      (is (no-prompt? state :corp) "Corp not prompted to trigger Strategic Innovations"))))

(deftest sync-everything-everywhere
  ;; SYNC: Everything, Everywhere
  (do-game
    (new-game {:corp {:id "SYNC: Everything, Everywhere"}
               :runner {:deck ["Fan Site"]}
               :options {:start-as :runner}})
    (play-from-hand state :runner "Fan Site")
    (gain-tags state :runner 1)
    (is (= 1 (count-tags state)) "Runner has 1 tag")
    (is (changed? [(:credit (get-runner)) -3]
          (remove-tag state :runner))
        "Paid 3c to remove tag")
    (is (= 0 (count-tags state)) "Runner removed tag")
    (take-credits state :runner)
    (gain-tags state :runner 1)
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (swap! state assoc-in [:corp :credit] 0)
    (is (changed? [(:credit (get-runner)) 0]
          (trash-resource state)
          (click-card state :corp (get-resource state 0)))
        "Paid 0c to trash resource")
    (is (= ["Fan Site"] (map :title (:discard (get-runner)))) "Trashed Fan Site")))

(deftest tao-salonga-telepresence-magician
  ;;Tāo Salonga: Telepresence Magician
  (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                       :hand ["Ice Wall" "Enigma" "House of Knives"]}
                :runner {:id "Tāo Salonga: Telepresence Magician"}})
     (play-from-hand state :corp "Ice Wall" "HQ")
     (play-from-hand state :corp "Enigma" "HQ")
     (play-from-hand state :corp "House of Knives" "New remote")
     (take-credits state :corp)
     (run-empty-server state "Server 1")
     (let [iw (get-ice state :hq 0)
           enig (get-ice state :hq 1)]
       (click-prompt state :runner "Steal")
       (click-prompt state :runner "Yes")
       (click-card state :runner (refresh enig))
       (click-card state :runner (refresh iw)))
     (let [iw (get-ice state :hq 1)
           enig (get-ice state :hq 0)]
       (is (= "Ice Wall" (:title iw)) "Ice Wall now outermost ice")
       (is (= "Enigma" (:title enig)) "Enigma now outermost ice"))))

(deftest tao-salonga-swapping-ice-doesnt-mess-with-their-strength
  ;;Tāo Salonga swapping Palisade, Ice Wall, or Rime doesn't mess with their strength
  (do-game
    (new-game {:corp {:hand ["Palisade" "Ice Wall" "Rime" (qty "House of Knives" 2)]}
               :runner {:id "Tāo Salonga: Telepresence Magician"}})
    (play-from-hand state :corp "Palisade" "Archives")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Rime" "Server 1")
    (take-credits state :corp)
    (let [palisade (get-ice state :archives 0)
          rime (get-ice state :remote1 1)]
      (rez state :corp palisade)
      (is (= 2 (core/get-strength (refresh palisade))))
      (rez state :corp rime)
      (is (= 1 (core/get-strength (refresh rime))))
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Yes")
      (click-card state :runner (refresh palisade))
      (click-card state :runner (refresh rime)))
    (let [palisade (get-ice state :remote1 1)
          iw (get-ice state :remote1 0)
          rime (get-ice state :archives 0)]
      (is (= 1 (core/get-strength (refresh rime))) "Rime retains strength")
      (is (= 4 (core/get-strength (refresh palisade))) "Palisade gains strength")
      (take-credits state :runner)
      (core/gain state :corp :credit 1)
      (rez state :corp iw)
      (is (= 1 (core/get-strength (refresh iw))))
      (advance state iw 1)
      (is (= 2 (core/get-strength (refresh iw))))
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Yes")
      (click-card state :runner (refresh iw))
      (click-card state :runner (refresh rime)))
    (is (= 2 (core/get-strength (refresh (get-ice state :archives 0)))) "Ice Wall retains strength")))

(deftest tao-salonga-swapping-ice-with-trojan-doesnt-reset-mu
  ;;Tāo Salonga swapping ice with trojan doesn't make the trojan mu-cost disappear
  (do-game
    (new-game {:corp {:hand ["Palisade" "Ice Wall" "House of Knives"]}
               :runner {:id "Tāo Salonga: Telepresence Magician"
                        :hand ["Saci"]}})
    (play-from-hand state :corp "Palisade" "Archives")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (take-credits state :corp)
    (let [palisade (get-ice state :archives 0)
          iw (get-ice state :remote1 0)]
      (play-from-hand state :runner "Saci")
      (click-card state :runner iw)
      (run-empty-server state "HQ")
      (is (changed? [(core/available-mu state) 0]
            (click-prompt state :runner "Steal")
            (click-prompt state :runner "Yes")
            (click-card state :runner (refresh palisade))
            (click-card state :runner (refresh iw)))
          "Available MU should not change"))))

(deftest the-foundry-refining-the-process-interaction-with-accelerated-beta-test
    ;; interaction with Accelerated Beta Test
    (do-game
      (new-game {:corp {:id "The Foundry: Refining the Process"
                        :deck [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)]}})
      (starting-hand state :corp ["Accelerated Beta Test"])
      (play-from-hand state :corp "Accelerated Beta Test" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "OK")
      (click-prompt state :corp "Eli 1.0")
      (click-prompt state :corp "Archives")
      (click-prompt state :corp "Yes")
      (is (empty? (:play-area (get-corp))) "Play area shuffled into R&D")
      (is (= 1 (count (:hand (get-corp)))) "Added Eli 1.0 to HQ")))

(deftest the-outfit-family-owned-and-operated
  ;; The Outfit - Gain 3 whenever you take at least 1 bad publicity
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

(deftest the-outfit-family-owned-and-operated-with-profiteering-only-gain-3-credits-when-taking-more-than-1-bad-publicity-in-a-single-effect
    ;; with Profiteering - Only gain 3 credits when taking more than 1 bad publicity in a single effect
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Profiteering"]}})
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "3")
      (is (= 3 (count-bad-pub state)) "Take 3 bad publicity")
      (is (= 23 (:credit (get-corp))) "Gain 15 from Profiteering + 3 from The Outfit")))

(deftest the-outfit-family-owned-and-operated-vs-valencia-1-bad-pub-at-start-means-5-credits-to-start-with-does-not-gain-bp
    ;; vs Valencia - 1 bad pub at start means 5 credits to start with (does not _gain_ BP)
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
      (is (= (+ 5 7 3) (:credit (get-corp))) "Gain 7 from Hostile Takeover + 3 from The Outfit")))

(deftest titan-transnational-investing-in-your-future-add-a-counter-to-a-scored-agenda
    ;; Add a counter to a scored agenda
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atl (get-content state :remote1 0)]
        (core/gain state :corp :click 1)
        (click-advance state :corp (refresh atl))
        (click-advance state :corp (refresh atl))
        (click-advance state :corp (refresh atl))
        (score state :corp (refresh atl))
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))

(deftest titan-transnational-investing-in-your-future-only-use-one-counter-of-corporate-sales-team
    ;; only use one counter of Corporate Sales Team
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Corporate Sales Team" "Mark Yale"]}})
      (play-from-hand state :corp "Corporate Sales Team" "New remote")
      (play-from-hand state :corp "Mark Yale" "New remote")
      (let [cst (get-content state :remote1 0)
            my (get-content state :remote2 0)]
        (core/gain state :corp :click 3)
        (click-advance state :corp (refresh cst))
        (click-advance state :corp (refresh cst))
        (click-advance state :corp (refresh cst))
        (click-advance state :corp (refresh cst))
        (score state :corp (refresh cst))
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
          (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
          (rez state :corp my)
          (card-ability state :corp my 1)
          (click-card state :corp (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
          (card-ability state :corp my 1)
          (is (no-prompt? state :corp) "No prompt for the Corp as no counters exist to spend")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")))))

(deftest thule-subsea-safety-below-pay
  (do-game
    (new-game {:corp {:id "Thule Subsea: Safety Below"
                      :deck ["Project Vitruvius"]}})
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (is (changed? [(:credit (get-runner)) -2
           (:click (get-runner)) -1]
          (click-prompt state :runner "Pay [Click] and 2 [Credits]"))
        "paid creds to not suffer")
    (is (= 0 (:brain-damage (get-runner))) "Runner took no core damage")))

(deftest thule-subsea-safety-below-cannot-pay
  (do-game
    (new-game {:corp {:id "Thule Subsea: Safety Below"
                      :deck ["Project Vitruvius"]}})
    (take-credits state :corp)
    (click-credit state :runner)
    (click-credit state :runner)
    (click-credit state :runner)
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:choices (prompt-map :runner)))))))

(deftest thule-subsea-safety-below-suffer
  (do-game
    (new-game {:corp {:id "Thule Subsea: Safety Below"
                      :deck ["Project Vitruvius"]}})
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (is (changed? [(:credit (get-runner)) 0
           (:click (get-runner)) 0]
          (click-prompt state :runner "Suffer 1 core damage"))
        "didn't pay credits")
    (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")))

(deftest thunderbolt-armaments
  (do-game
    (new-game {:corp {:id "Thunderbolt Armaments: Peace Through Power"
                      :deck ["Tithe" "Swordsman" "Vanilla"]}
               :runner {:hand ["Smartware Distributor"]}})
    (play-from-hand state :corp "Tithe" "HQ")
    (play-from-hand state :corp "Swordsman" "R&D")
    (play-from-hand state :corp "Vanilla" "Archives")
    (let [tithe (get-ice state :hq 0)
          sw (get-ice state :rd 0)
          van (get-ice state :archives 0)]
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Smartware Distributor")
      (run-on state :archives)
      (rez state :corp van)
      (run-continue state)
      (is (= 0 (get-strength (refresh van))) "No strength buff to Vanilla")
      (fire-subs state van)
      (run-on state :hq)
      (rez state :corp tithe)
      (run-continue state)
      (is (= 2 (get-strength (refresh tithe))) "Tithe strength is buffed")
      (is (= 3 (count (:subroutines (refresh tithe)))) "Tithe has 3 subroutines")
      (card-subroutine state :corp tithe 2)
      (click-prompt state :runner "End the run")
      (run-on state :rd)
      (rez state :corp sw)
      (run-continue state)
      (is (= 3 (get-strength (refresh sw))) "Swordsman strength is buffed")
      (is (= 3 (count (:subroutines (refresh sw)))) "Swordsman has 3 subroutines")
      (card-subroutine state :corp sw 2)
      (click-prompt state :runner "Trash 1 installed card")
      (click-card state :runner "Smartware Distributor")
      (is (:run @state) "Run continues"))))

(deftest weyland-consortium-because-we-built-it-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Because We Built It"
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)
            bwbi (get-in @state [:corp :identity])]
        (is (changed? [(:credit (get-corp)) 0]
              (click-advance state :corp (refresh iw))
              (click-card state :corp bwbi))
            "Used 1 credit from Weyland BWBI to advance Ice Wall"))))

(deftest weyland-consortium-builder-of-nations-1-meat-damage-per-turn-at-most
    ;; 1 meat damage per turn at most
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand [(qty "Sure Gamble" 5)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (advance state (get-ice state :hq 0) 1)
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 meat damage from BoN")
      (run-jack-out state)
      (run-on state "HQ")
      (run-continue state)
      (run-continue state)
      (is (= 1 (count (:discard (get-runner)))) "Runner took only 1 meat damage from BoN total")
      (is (prompt-is-type? state :corp :run) "Only run prompt is active")))

(deftest weyland-consortium-builder-of-nations-2-meat-damage-from-id-ability-when-the-cleaners-is-scored
    ;; 2 meat damage from ID ability when The Cleaners is scored
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
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from BoN/Cleaners combo")))

(deftest weyland-consortium-builder-of-nations-trashing-a-solo-ice-on-an-empty-server-still-triggers-5025
    ;; trashing a solo ice on an empty server still triggers #5025
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
      (rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (is (nil? (get-ice state :remote1 0)) "Ice Wall is trashed")
      (is (nil? (:run @state)) "Ice Wall is trashed, so run has been ended")
      (is (= 1 (count (:discard (get-runner)))))))

(deftest weyland-consortium-builder-of-nations-doesn-t-trigger-if-id-was-blank-during-first-end-of-encounter-5594
    ;; doesn't trigger if ID was blank during first end of encounter #5594
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand [(qty "Direct Access" 5)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (advance state (get-ice state :hq 0) 1)
      (take-credits state :corp)
      (play-from-hand state :runner "Direct Access")
      (click-prompt state :runner "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (zero? (count (:discard (get-runner)))) "Runner takes no damage")
      (run-jack-out state)
      (click-prompt state :runner "Yes")
      (run-on state "HQ")
      (run-continue state)
      (run-continue state)
      (is (zero? (count (:discard (get-runner)))) "Runner takes no damage at the second encounter")
      (is (no-prompt? state :corp))))

(deftest weyland-consortium-built-to-last
  ;; Weyland Consortium: Built to Last
  (do-game
      (new-game {:corp {:id "Weyland Consortium: Built to Last"
                        :hand [(qty "NGO Front" 2) "Oaktown Renovation"]}})
      (core/gain state :corp :click 5)
      (play-from-hand state :corp "NGO Front" "New remote")
      (play-from-hand state :corp "NGO Front" "New remote")
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (let [ngo1 (get-content state :remote1 0)
            ngo2 (get-content state :remote2 0)
            oaktown (get-content state :remote3 0)]
        (is (changed? [(:credit (get-corp)) 3]
              (advance state (refresh oaktown) 1))
            "Gain 2 + 2 - 1 = 3 credits from Weyland Built to Last + Oaktown ability")
        (is (changed? [(:credit (get-corp)) 0]
              (advance state (refresh ngo1) 1)
              (advance state (refresh ngo1) 1))
            "Gain 2 - 1 - 1 = 0 credits net from double advancing Weyland Built to Last ability")
        (is (changed? [(:credit (get-corp)) 1]
              (advance state (refresh ngo2) 1))
            "Gain 2 - 1 = 1 credits from Weyland Built to Last ability"))))

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

(deftest wyvern-chemically-enhanced-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp   {:deck [(qty "Launch Campaign" 3)]}
                 :runner {:id   "Wyvern: Chemically Enhanced"
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
      (click-prompt state :runner "Pay 2 [Credits] to trash") ;; trash Launch Campaign, should trigger wyvern
      (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
          "Sure Gamble still in Wyvern's discard")
      (is (some #(= "Easy Mark" (:title %)) (:deck (get-runner))) "Easy Mark moved to deck")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (= "Sure Gamble" (:title (last (:discard (get-runner))))) "Sure Gamble still in Wyvern's discard")))

(deftest wyvern-chemically-enhanced-blacklist-rezzed
    ;; Blacklist Rezzed
    (do-game
      (new-game {:corp   {:deck [(qty "Launch Campaign" 3) "Blacklist"]}
                 :runner {:id   "Wyvern: Chemically Enhanced"
                          :deck [(qty "Sure Gamble" 2) "Corroder"
                                 "Clone Chip" "Easy Mark"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote2 0)))
      (take-credits state :corp)
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Corroder")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 2 [Credits] to trash") ;; trash Launch Campaign, should trigger wyvern
      (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
          "Sure Gamble still in Wyvern's discard")))

(deftest zahya-sadeghi-versatile-smuggler
  ;; "Zahya Sadeghi: Versatile Smuggler"
  (do-game
     (new-game {:corp   {:hand [(qty "Hedge Fund" 3)]}
                :runner {:id   "Zahya Sadeghi: Versatile Smuggler"
                         :hand [(qty "HQ Interface" 2)]
                         :credits 20}})
     (take-credits state :corp)
     (play-from-hand state :runner "HQ Interface")
     (play-from-hand state :runner "HQ Interface")
     (run-empty-server state :hq)
     (click-prompt state :runner "No action")
     (click-prompt state :runner "No action")
     (click-prompt state :runner "No action")
     (is (changed? [(:credit (get-runner)) 3]
           (click-prompt state :runner "Yes"))
         "Gain 3 credits from ID ability")))
