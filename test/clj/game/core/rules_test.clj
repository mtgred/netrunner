(ns game.core.rules-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest corp-rez-unique
  ;; Rezzing a second copy of a unique Corp card
  (do-game
    (new-game {:corp {:deck [(qty "Caprice Nisei" 2)]}})
    (play-from-hand state :corp "Caprice Nisei" "HQ")
    (play-from-hand state :corp "Caprice Nisei" "R&D")
    (rez state :corp (get-content state :hq 0))
    (is (:rezzed (get-content state :hq 0)) "First Caprice rezzed")
    (rez state :corp (get-content state :rd 0) {:expect-rez false})
    (is (not (:rezzed (get-content state :rd 0))) "Second Caprice could not be rezzed")))

(deftest runner-install-program
  ;; runner-install - Program; ensure costs are paid
  (do-game
    (new-game {:runner {:deck ["Gordian Blade"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (let [gord (get-program state 0)]
      (is (= (- 5 (:cost gord)) (:credit (get-runner))) "Program cost was applied")
      (is (= (- 4 (:memoryunits gord)) (core/available-mu state)) "Program MU was applied"))))

(deftest installing-second-unique-trashes-first-unique-test
  (do-game
    (new-game {:runner {:hand [(qty "Kati Jones" 2)]
                        :credits 100}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (play-from-hand state :runner "Kati Jones")
    (is (find-card "Kati Jones" (get-resource state)))
    (is (last-log-contains? state "Kati Jones is trashed."))))

(deftest installing-second-unique-on-off-campus-apartment-trashes-first-test
  (do-game
    (new-game {:runner {:hand [(qty "Kati Jones" 2) "Off-Campus Apartment"]
                        :credits 100}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (play-from-hand state :runner "Off-Campus Apartment")
    (let [oca (get-resource state 1)]
      (card-ability state :runner oca 0)
      (click-card state :runner (find-card "Kati Jones" (:hand (get-runner))))
      (is (find-card "Kati Jones" (:hosted (refresh oca))))
      (is (= "Kati Jones" (:title (get-discarded state :runner))))
      (is (last-log-contains? state "Kati Jones is trashed.")))))

(deftest installing-second-hivemind-trashes-hosted-hivemind-test
  (do-game
    (new-game {:runner {:hand ["Scheherazade" (qty "Hivemind" 2)]
                        :credits 100}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scheherazade")
    (let [scheh (get-program state 0)]
      (card-ability state :runner scheh 0)
      (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
      (is (find-card "Hivemind" (:hosted (refresh scheh))) "Hivemind hosted on Scheherazade")
      (play-from-hand state :runner "Hivemind")
      (is (= "Hivemind" (:title (get-discarded state :runner))))
      (is (last-log-contains? state "Hivemind hosted on Scheherazade is trashed."))
      (is (empty? (:hosted (refresh scheh))) "Hivemind hosted on Scheherazade"))))

(deftest deactivate-program
  ;; deactivate - Program; ensure MU are restored
  (do-game
    (new-game {:runner {:deck ["Gordian Blade"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (let [gord (get-program state 0)]
      (trash state :runner gord)
      (is (= 4 (core/available-mu state)) "Trashing the program restored MU"))))

(deftest agenda-forfeit-runner
  ;; forfeit - Don't deactivate agenda to trigger leave play effects if Runner forfeits a stolen agenda
  (do-game
    (new-game {:corp {:deck ["Mandatory Upgrades"]}
               :runner {:deck ["Data Dealer"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Data Dealer")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (is (= 2 (:agenda-point (get-runner))))
    (card-ability state :runner (get-resource state 0) 0)
    (click-card state :runner (get-scored state :runner 0))
    (is (= 1 (:click (get-runner))) "Didn't lose a click")
    (is (= 4 (:click-per-turn (get-runner))) "Still have 4 clicks per turn")))

(deftest agenda-forfeit-corp
  ;; forfeit - Deactivate agenda to trigger leave play effects if Corp forfeits a scored agenda
  (do-game
    (new-game {:corp {:deck ["Mandatory Upgrades" "Corporate Town"]}})
    (play-from-hand state :corp "Mandatory Upgrades" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
    (play-from-hand state :corp "Corporate Town" "New remote")
    (let [ctown (get-content state :remote2 0)]
      (rez state :corp ctown {:expect-rez false})
      (click-card state :corp (get-scored state :corp 0))
      (is (= 3 (:click-per-turn (get-corp))) "Back down to 3 clicks per turn"))))

(deftest refresh-recurring-credits-hosted
  ;; host - Recurring credits on cards hosted after install refresh properly
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Hedge Fund" 3)]}
               :runner {:deck ["Compromised Employee" "Off-Campus Apartment"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Off-Campus Apartment")
    (let [iwall (get-ice state :hq 0)
          apt (get-resource state 0)]
      (card-ability state :runner apt 0) ; use Off-Campus option to host a card
      (click-card state :runner "Compromised Employee")
      (let [cehosted (first (:hosted (refresh apt)))]
        (card-ability state :runner cehosted 0) ; take Comp Empl credit
        (is (= 4 (:credit (get-runner))))
        (is (zero? (get-counters (refresh cehosted) :recurring)))
        (rez state :corp iwall)
        (is (= 5 (:credit (get-runner))) "Compromised Employee gave 1 credit from ice rez")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh cehosted) :recurring))
            "Compromised Employee recurring credit refreshed")))))

(deftest card-str-test-simple
  ;; ensure card-str names cards in simple situations properly
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Jackson Howard" 2)]}
               :runner {:deck ["Corroder" "Clone Chip" "Paparazzi" "Parasite"]}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (end-turn state :corp)
    (start-turn state :runner)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Paparazzi")
    (play-from-hand state :runner "Parasite")
    (let [hqiwall0 (get-ice state :hq 0)
          hqiwall1 (get-ice state :hq 1)
          rdiwall (get-ice state :rd 0)
          jh1 (get-content state :remote1 0)
          jh2 (get-content state :remote2 0)
          corr (get-program state 0)
          cchip (get-hardware state 0)
          pap (get-resource state 0)]
      (rez state :corp hqiwall0)
      (rez state :corp jh1)
      (click-card state :runner (refresh hqiwall0))
      (is (= (core/card-str state (refresh hqiwall0)) "Ice Wall protecting HQ at position 0"))
      (is (= (core/card-str state (refresh hqiwall1)) "ice protecting HQ at position 1"))
      (is (= (core/card-str state (refresh rdiwall)) "ice protecting R&D at position 0"))
      (is (= (core/card-str state (refresh rdiwall) {:visible true})
             "Ice Wall protecting R&D at position 0"))
      (is (= (core/card-str state (refresh jh1)) "Jackson Howard in Server 1"))
      (is (= (core/card-str state (refresh jh2)) "a card in Server 2"))
      (is (= (core/card-str state (refresh corr)) "Corroder"))
      (is (= (core/card-str state (refresh cchip)) "Clone Chip"))
      (is (= (core/card-str state (refresh pap)) "Paparazzi"))
      (is (= (core/card-str state (first (:hosted (refresh hqiwall0))))
             "Parasite hosted on Ice Wall protecting HQ at position 0")))))

(deftest invalid-score-attempt
  ;; Test scoring with an incorrect number of advancement tokens
  (do-game
    (new-game {:corp {:deck ["Ancestral Imager"]}})
    (play-from-hand state :corp "Ancestral Imager" "New remote")
    (let [ai (get-content state :remote1 0)]
      ;; Trying to score without any tokens does not do anything
      (is (not (find-card "Ancestral Imager" (:scored (get-corp)))) "AI not scored")
      (is (not (nil? (get-content state :remote1 0))))
      (click-advance state :corp (refresh ai))
      (score state :corp (refresh ai))
      (is (not (nil? (get-content state :remote1 0)))))))

(deftest trash-corp-hosted
  ;; Hosted Corp cards are included in all-installed and fire leave-play effects when trashed
  (do-game
    (new-game {:corp {:deck ["Full Immersion RecStudio" "Worlds Plaza" "Director Haas"]}})
    (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
    (let [fir (get-content state :remote1 0)]
      (rez state :corp fir)
      (card-ability state :corp fir 0)
      (click-card state :corp (find-card "Worlds Plaza" (:hand (get-corp))))
      (let [wp (first (:hosted (refresh fir)))]
        (rez state :corp wp)
        (card-ability state :corp wp 0)
        (click-card state :corp (find-card "Director Haas" (:hand (get-corp))))
        (let [dh (first (:hosted (refresh wp)))]
          (is (:rezzed dh) "Director Haas was rezzed")
          (is (zero? (:credit (get-corp))) "Corp has 0 credits")
          (is (= 4 (:click-per-turn (get-corp))) "Corp has 4 clicks per turn")
          (is (= 3 (count (core/all-installed state :corp))) "all-installed counting hosted Corp cards")
          (take-credits state :corp)
          (run-empty-server state "Server 1")
          (click-card state :runner dh)
          (click-prompt state :runner "Pay 5 [Credits] to trash") ; trash Director Haas
          (click-prompt state :runner "Done")
          (is (= 3 (:click-per-turn (get-corp))) "Corp down to 3 clicks per turn"))))))

(deftest trash-remove-per-turn-restriction
  ;; Trashing a card should remove it from [:per-turn] - Issue #1345
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
               :runner {:deck [(qty "Imp" 2) "Scavenge"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "Imp")
    (let [imp (get-program state 0)]
      (run-empty-server state "HQ")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (is (= 1 (count (:discard (get-corp)))) "Accessed Hedge Fund is trashed")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "Card can't be trashed, Imp already used this turn")
      (play-from-hand state :runner "Scavenge")
      (click-card state :runner imp)
      (click-card state :runner (find-card "Imp" (:discard (get-runner)))))
    (let [imp (get-program state 0)]
      (is (= 2 (get-counters (refresh imp) :virus)) "Reinstalled Imp has 2 counters")
      (run-empty-server state "HQ")
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card"))
    (is (= 2 (count (:discard (get-corp)))) "Hedge Fund trashed, reinstalled Imp used on same turn")))

(deftest trash-seen-and-unseen
  ;; Trash installed assets that are both seen and unseen by runner
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 7)]
                      :hand ["PAD Campaign" "Sandburg" "NGO Front"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Sandburg" "New remote")
    (take-credits state :corp 1)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "No action")
    ;; run and trash the second asset
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Pay 4 [Credits] to trash")
    (take-credits state :runner 2)
    (play-from-hand state :corp "NGO Front" "Server 1")
    (is (= "The PAD Campaign in Server 1 will now be trashed." (:msg (prompt-map :corp))))
    (click-prompt state :corp "OK")
    (is (= 2 (count (:discard (get-corp)))) "Trashed existing asset")
    (is (:seen (find-card "Sandburg" (:discard (get-corp)))) "Asset trashed by runner is Seen")
    (is (not (:seen (find-card "PAD Campaign" (:discard (get-corp)))))
        "Asset trashed by corp is Unseen")
    (is (not (:seen (get-content state :remote1 0))) "New asset is unseen")))

(deftest reinstall-seen-asset
  ;; Install a faceup card in Archives, make sure it is not :seen
  (do-game
    (new-game {:corp {:deck ["PAD Campaign" "Interns"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp 2)
    ;; run and trash the asset
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 4 [Credits] to trash")
    (is (:seen (first (get-in @state [:corp :discard]))) "Asset trashed by runner is Seen")
    (take-credits state :runner 3)
    (play-from-hand state :corp "Interns")
    (click-card state :corp (first (get-in @state [:corp :discard])))
    (click-prompt state :corp "New remote")
    (is (not (:seen (get-content state :remote2 0))) "New asset is unseen")))

(deftest all-installed-runner-test
  ;; Tests all-installed for programs hosted on ice, nested hosted programs, and non-installed hosted programs
  (do-game
    (new-game {:corp {:deck ["Wraparound"]}
               :runner {:deck ["Omni-drive" "Personal Workshop" "Leprechaun" "Corroder" "Mimic" "Knight"]}})
    (play-from-hand state :corp "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (rez state :corp wrap)
      (take-credits state :corp)
      (draw state :runner)
      (core/gain state :runner :credit 7)
      (play-from-hand state :runner "Knight")
      (play-from-hand state :runner "Personal Workshop")
      (play-from-hand state :runner "Omni-drive")
      (take-credits state :corp)
      (let [kn (get-program state 0)
            pw (get-resource state 0)
            od (get-hardware state 0)
            co (find-card "Corroder" (:hand (get-runner)))
            le (find-card "Leprechaun" (:hand (get-runner)))]
        (card-ability state :runner kn 0)
        (click-card state :runner wrap)
        (card-ability state :runner pw 0)
        (click-card state :runner co)
        (card-ability state :runner od 0)
        (click-card state :runner le)
        (let [od (refresh od)
              le (first (:hosted od))
              mi (find-card "Mimic" (:hand (get-runner)))]
          (card-ability state :runner le 0)
          (click-card state :runner mi)
          (let [all-installed (core/all-installed state :runner)]
            (is (= 5 (count all-installed)) "Number of installed runner cards is correct")
            (is (not-empty (filter #(= (:title %) "Leprechaun") all-installed)) "Leprechaun is in all-installed")
            (is (not-empty (filter #(= (:title %) "Personal Workshop") all-installed)) "Personal Workshop is in all-installed")
            (is (not-empty (filter #(= (:title %) "Mimic") all-installed)) "Mimic is in all-installed")
            (is (not-empty (filter #(= (:title %) "Omni-drive") all-installed)) "Omni-drive is in all-installed")
            (is (not-empty (filter #(= (:title %) "Knight") all-installed)) "Knight is in all-installed")
            (is (empty? (filter #(= (:title %) "Corroder") all-installed)) "Corroder is not in all-installed")))))))

(deftest log-accessed-names
  ;; Check that accessed card names are logged - except those on R&D, and no logs on archives
  (do-game
    (new-game {:corp {:deck [(qty "PAD Campaign" 7)]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (trash-from-hand state :corp "PAD Campaign")
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :rd)
    (click-prompt state :runner "No action")
    (is (last-log-contains? state "an unseen card") "Accessed card name was not logged")
    (run-empty-server state :remote1)
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")))

(deftest run-bad-publicity-credits
  ;; Should not lose BP credits until a run is completely over. Issue #1721.
  (do-game
    (new-game {:corp {:deck [(qty "Cyberdex Virus Suite" 3)]}
               :runner {:id "Valencia Estevez: The Angel of Cayambe"
                        :deck [(qty "Sure Gamble" 3)]}})
    (is (= 1 (count-bad-pub state)) "Corp starts with 1 BP")
    (play-from-hand state :corp "Cyberdex Virus Suite" "New remote")
    (play-from-hand state :corp "Cyberdex Virus Suite" "R&D")
    (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :corp "No")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (= 5 (:credit (get-runner))) "1 BP credit spent to trash CVS")
    (run-empty-server state :hq)
    (click-prompt state :corp "No")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (= 5 (:credit (get-runner))) "1 BP credit spent to trash CVS")
    (run-empty-server state :rd)
    (click-prompt state :corp "No")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (= 5 (:credit (get-runner))) "1 BP credit spent to trash CVS")))

(deftest run-psi-bad-publicity-credits
  ;; Should pay from Bad Pub for Psi games during run #2374
  (do-game
    (new-game {:corp {:deck [(qty "Caprice Nisei" 3)]}
               :runner {:id "Valencia Estevez: The Angel of Cayambe"
                        :deck [(qty "Sure Gamble" 3)]}})
    (is (= 1 (count-bad-pub state)) "Corp starts with 1 BP")
    (play-from-hand state :corp "Caprice Nisei" "New remote")
    (take-credits state :corp)
    (let [caprice (get-content state :remote1 0)]
      (rez state :corp caprice)
      (run-on state "Server 1")
      (is (prompt-is-card? state :corp caprice) "Caprice prompt even with no ice, once runner makes run")
      (is (prompt-is-card? state :runner caprice) "Runner has Caprice prompt")
      (click-prompt state :corp "2 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-runner))) "Runner spend bad pub credit on psi game")
      (is (= 3 (:credit (get-corp))) "Corp spent 2 on psi game"))))

(deftest purge-nested
  ;; Purge nested-hosted virus counters
  (do-game
    (new-game {:corp {:deck ["Cyberdex Trial"]}
               :runner {:deck ["Djinn" "Imp" "Leprechaun"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Leprechaun")
    (let [lep (get-program state 0)]
      (card-ability state :runner lep 0)
      (click-card state :runner (find-card "Djinn" (:hand (get-runner))))
      (let [djinn (first (:hosted (refresh lep)))]
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Imp" (:hand (get-runner))))
        (let [imp (first (:hosted (refresh djinn)))]
          (is (= 2 (get-counters imp :virus)) "Imp has 2 virus counters")
          (take-credits state :runner)
          (play-from-hand state :corp "Cyberdex Trial")
          (is (zero? (get-counters (refresh imp) :virus)) "Imp counters purged"))))))

(deftest purge-corp
  ;; Purge virus counters on Corp cards
  (do-game
    (new-game {:corp {:deck ["Cyberdex Trial" "Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (core/command-counter state :corp ["virus" 2])
      (click-card state :corp iw)
      (is (= 2 (get-counters (refresh iw) :virus)) "2 counters on Ice Wall")
      (play-from-hand state :corp "Cyberdex Trial")
      (take-credits state :corp)
      (is (= 0 (get-counters (refresh iw) :virus)) "Purging removed Ice Wall counters"))))

(deftest end-the-run-test
  ;; Since all ETR ice share a common ability, we only need one test
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Restructure" 2)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp 2)
    (run-on state "HQ")
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-ice state :hq 0)]
      (rez state :corp iwall)
      (run-continue state)
      (card-subroutine state :corp iwall 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:runner :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest auto-pump-breakers-single-pump
    ;; Single pump
    (do-game
      (new-game {:corp {:deck ["Masvingo"]}
                 :runner {:deck ["Laamb"]}})
      (play-from-hand state :corp "Masvingo" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Laamb")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (let [laamb (get-program state 0)]
        (is (= 2 (get-strength (refresh laamb))) "Laamb starts at 2 strength")
        (is (= 6 (:credit (get-runner))) "Spent 4 to install")
        (auto-pump state (refresh laamb))
        (is (= 8 (get-strength (refresh laamb))) "Laamb is at 8 strength")
        (is (= 3 (:credit (get-runner))) "Spent 3 to pump"))))

(deftest auto-pump-breakers-multi-pump
    ;; Multi pump
    (do-game
      (new-game {:corp {:deck ["Masvingo"]}
                 :runner {:deck ["Ankusa"]}})
      (play-from-hand state :corp "Masvingo" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Ankusa")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (let [ank (get-program state 0)]
        (is (zero? (get-strength (refresh ank))) "Ankusa starts at 1 strength")
        (is (= 4 (:credit (get-runner))) "Spent 6 to install")
        (auto-pump state (refresh ank))
        (is (= 3 (get-strength (refresh ank))) "Ankusa is at 3 strength")
        (is (= 1 (:credit (get-runner))) "Spent 3 to pump"))))

(deftest autoresolve-aeneas-with-and-without-autoresolve
    ;; Aeneas with and without autoresolve
    (do-game
     (new-game {:corp {:deck ["Jackson Howard"]}
                :runner {:deck [(qty "Aeneas Informant" 2)]}})
     (play-from-hand state :corp "Jackson Howard" "New remote")
     (take-credits state :corp)
     (core/gain state :runner :click 50)
     (play-from-hand state :runner "Aeneas Informant")
     (letfn [(run-jackson []
               (run-empty-server state "Server 1")
               (click-prompt state :runner "No action"))
             (get-aeneas1 [] (get-resource state 0))]
       ;; before we toggle anything, aeneas should always prompt, and after we set it to 'Ask',
       ;; it should still do so, even if it was already set to 'Ask'
       (dotimes [_ 3]
         (run-jackson)
         (is (changed? [(:credit (get-runner)) 1]
               ; triggering Aeneas should grant a credit
               (click-prompt state :runner "Yes")))
         (is (no-prompt? state :runner) "No Aeneas prompt displaying")
         (run-jackson)
         (is (changed? [(:credit (get-runner)) 0]
               ; not triggering Aeneas should not grant a credit
               (click-prompt state :runner "No")))
         (is (no-prompt? state :runner) "No Aeneas prompt displaying")
         (card-ability state :runner (get-aeneas1) 0)
         (click-prompt state :runner "Ask"))
       ;; if aeneas is set to always/never fire, we should get to run without being prompted
       (card-ability state :runner (get-aeneas1) 0)
       (click-prompt state :runner "Never")
       (is (changed? [(:credit (get-runner)) 0]
             (run-jackson)))
       (is (no-prompt? state :runner) "No Aeneas prompt displaying")
       (card-ability state :runner (get-aeneas1) 0)
       (click-prompt state :runner "Always")
       (is (changed? [(:credit (get-runner)) 1]
             (run-jackson)))
       (is (no-prompt? state :runner) "No Aeneas prompt displaying")
       ;; should also be able to play a new aeneas which doesn't care about the first one's autoresolve
       (play-from-hand state :runner "Aeneas Informant")
       (is (changed? [(:credit (get-runner)) 2]
             (run-jackson)
             (click-prompt state :runner "Yes")))
       (is (no-prompt? state :runner) "No Aeneas prompt displaying")
       (card-ability state :runner (get-resource state 1) 0)
       (click-prompt state :runner "Never")
       (is (changed? [(:credit (get-runner)) 1]
             (run-jackson)))
       (is (no-prompt? state :runner) "No Aeneas prompt displaying"))))

(deftest autoresolve-fisk-ftt-with-and-without-autoresolve
    ;; Fisk + FTT with and without autoresolve
    (do-game
     (new-game {:corp {:deck [(qty "Archer" 30)]}
                :runner {:id "Laramy Fisk: Savvy Investor"
                         :deck ["Find the Truth"]}})
     (take-credits state :corp)
     (click-card state :corp (first (:hand (get-corp))))
     (play-from-hand state :runner "Find the Truth")
     (letfn [(set-ftt-autoresolve [setting]
               (card-ability state :runner (get-resource state 0) 0)
               (click-prompt state :runner setting))
             (set-fisk-autoresolve [setting]
               (card-ability state :runner (get-in @state [:runner :identity]) 0)
               (click-prompt state :runner setting))
             (pass-turn-runner-corp []
               (take-credits state :runner)
               (starting-hand state :corp ["Archer"])
               (take-credits state :corp))]
       ;; with nothing done, ftt and fisk will both want to prompt on a successful central run, so will need to be ordered
       ;; this will remain the case after one of them is set to 'Ask'
       (run-empty-server state "Archives")
       (is (changed? [(count (get-in @state [:corp :hand])) 1]
             (click-prompt state :runner "Laramy Fisk: Savvy Investor")
             (click-prompt state :runner "Yes"))
           "Corp drew 1 card")
       ;; resolve FTT
       (click-prompt state :runner "Yes")
       (click-prompt state :runner "OK")
       (set-fisk-autoresolve "Ask")
       (pass-turn-runner-corp)
       (run-empty-server state "Archives")
       (is (changed? [(count (get-in @state [:corp :hand])) 1]
             (click-prompt state :runner "Laramy Fisk: Savvy Investor")
             (click-prompt state :runner "Yes"))
           "Corp drew 1 card")
       ;; resolve FTT
       (click-prompt state :runner "Yes")
       (click-prompt state :runner "OK")
       (set-fisk-autoresolve "Ask")
       (pass-turn-runner-corp)
       ;; if either is set to 'never', we should not need simult event resolution
       (set-fisk-autoresolve "Ask")
       (set-ftt-autoresolve "Never")
       (is (no-prompt? state :runner) "No prompts displaying")
       (run-empty-server state "Archives")
       (is (= "Laramy Fisk: Savvy Investor" (-> @state :runner :prompt first :card :title)) "Fisk prompt is open")
       (click-prompt state :runner "No")
       (is (no-prompt? state :runner) "No prompts displaying")
       (pass-turn-runner-corp)
       ;; if one is 'never' and the other is 'always', still do not need simult resolution
       (set-fisk-autoresolve "Never")
       (set-ftt-autoresolve "Always")
       (run-empty-server state "Archives")
       (click-prompt state :runner "OK")
       (is (no-prompt? state :runner) "No prompts displaying")
       (pass-turn-runner-corp)
       ;; if one is set to 'always', and the other to 'Ask' we do need simult event resolution
       (set-fisk-autoresolve "Always")
       (set-ftt-autoresolve "Ask")
       (run-empty-server state "Archives")
       (click-prompt state :runner "Find the Truth")
       (click-prompt state :runner "Yes")
       (is (changed? [(count (get-in @state [:corp :hand])) 1]
             (click-prompt state :runner "OK"))
           "Fisk triggers after closing FTT prompt")
       (is (no-prompt? state :runner) "No prompts displaying"))))

(deftest autoresolve-ensure-autoresolve-does-not-break-prompts-with-a-req
    ;; Ensure autoresolve does not break prompts with a :req
    (do-game
     (new-game {:corp {:id "SSO Industries: Fueling Innovation"
                       :deck ["Underway Renovation" (qty "Ice Wall" 3)]}})
     (letfn [(toggle-sso [setting]
               (card-ability state :corp (get-in @state [:corp :identity]) 0)
               (click-prompt state :corp setting))]
       (toggle-sso "Always")
       (play-from-hand state :corp "Underway Renovation" "New remote")
       (take-credits state :corp)
       (is (no-prompt? state :corp) "No prompts displaying, as conditions are not satisfied")
       (take-credits state :runner)
       (play-from-hand state :corp "Ice Wall" "New remote")
       (toggle-sso "Never")
       (take-credits state :corp)
       (is (no-prompt? state :corp) "No prompts displaying, as conditions are not satisfied")
       (take-credits state :runner)
       (toggle-sso "Always")
       (take-credits state :corp)
       (is (= "Choose a piece of ice with no advancement tokens to place 1 advancement token on"
              (:msg (get-prompt state :corp))) "SSO autoresolved first prompt")
       (click-card state :corp (get-ice state :remote2 0))
       (is (= 1 (get-counters (get-ice state :remote2 0) :advancement)) "A token was added")
       (is (no-prompt? state :corp) "No prompt displaying")
       (take-credits state :runner)
       (take-credits state :corp)
       (is (no-prompt? state :corp) "No prompt displaying, as conditions are not met"))))

(deftest autoresolve-ctm-autoresolve
    ;; CtM autoresolve
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck [(qty "Rashida Jaheem" 3)]}})
      (letfn [(toggle-ctm [setting]
                (card-ability state :corp (get-in @state [:corp :identity]) 0)
                (click-prompt state :corp setting))]
        (play-from-hand state :corp "Rashida Jaheem" "New remote")
        (play-from-hand state :corp "Rashida Jaheem" "New remote")
        (play-from-hand state :corp "Rashida Jaheem" "New remote")
        (take-credits state :corp)
        (toggle-ctm "Ask")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (take-credits state :runner)
        (take-credits state :corp)
        (toggle-ctm "Always")
        (run-empty-server state "Server 2")
        (click-prompt state :runner "Pay 1 [Credits] to trash")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (no-prompt? state :corp) "No prompt displaying for Corp")
        (is (no-prompt? state :runner) "No prompt displaying for Runner"))))

(deftest no-scoring-after-terminal
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["IPO" "Hostile Takeover"]
                      :credits 15}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (advance state ht 2)
      (let [credits (:credit (get-corp))]
        (play-from-hand state :corp "IPO")
        (is (= (+ 5 credits) (:credit (get-corp))) "Corp gains 5 credits from IPO"))
      (score state :corp (refresh ht))
      (is (refresh ht) "Hostile Takeover isn't scored because a terminal Operation was played")
      (take-credits state :corp)
      (take-credits state :runner)
      (score state :corp (refresh ht))
      (is (nil? (refresh ht)) "Hostile Takeover is scored because it's the Corp's turn again")
      (is (= "Hostile Takeover" (:title (get-scored state :corp 0)))))))

(deftest clearing-currents
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Infrastructure" "Surveillance Sweep" "Hostile Takeover"]
                      :credits 10}
               :runner {:id "Leela Patel: Trained Pragmatist"
                        :hand ["Corporate \"Grant\"" (qty "Sure Gamble" 2)]}})
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Surveillance Sweep")
    (take-credits state :corp)
    (play-from-hand state :runner "Corporate \"Grant\"")
    (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage from clearing current")))

(deftest simultaneous-trash-effects
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Infrastructure" "Marilyn Campaign" "Calvin B4L3Y"]
                      :credits 20}
               :runner {:hand ["Apocalypse"]}})
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (play-from-hand state :corp "Marilyn Campaign" "New remote")
    (play-from-hand state :corp "Calvin B4L3Y" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (rez state :corp (get-content state :remote2 0))
    (rez state :corp (get-content state :remote3 0))
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Apocalypse")
    (is (= #{"Hostile Infrastructure" "Marilyn Campaign" "Calvin B4L3Y"}
           (into #{} (prompt-titles :corp)))
        "Corp has the simultaneous prompt")
    (click-prompt state :corp "Marilyn Campaign")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Calvin B4L3Y")
    (click-prompt state :corp "Yes")))

(deftest events-after-derez
  ;; If a card has an event triggered but an earlier event derezzed the card,
  ;; the event should not happen, as the event is only pending.
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Kakugo"]}
               :runner {:hand ["Saker"]
                        :credits 10}})
    (play-from-hand state :corp "Kakugo" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Saker")
    (run-on state "HQ")
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "End the run")
    (card-ability state :runner (get-program state 0) 2)
    (run-continue state)
    (is (= 1 (-> (get-runner) :hand count)) "Saker is still in hand")))

(deftest start-of-turn-phase-12
  ;; :corp-phase-12 and :runner-phase-12 should look at derezzed cards as well
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Rashida Jaheem"]}
               :runner {:hand ["Security Testing"]}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Security Testing")
    (core/update! state :runner (assoc (get-resource state 0) :facedown true))
    (take-credits state :runner)
    (is (:corp-phase-12 @state) "Facedown corp cards can be rezzed so trigger phase 1.2")
    (end-phase-12 state :corp)
    (take-credits state :corp)
    (is (not (:runner-phase-12 @state)) "Facedown runner cards can't be turned faceup")))

(deftest move-removes-icon
  ;; Moving a marked ice to HQ removes icon #5196
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Project Yagi-Uda" "Ice Wall" "Enigma"]
                      :credits 10}
               :runner {:hand ["Boomerang"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-and-score state "Project Yagi-Uda")
    (take-credits state :corp)
    (play-from-hand state :runner "Boomerang")
    (let [icew (get-ice state :hq 0)
          yagi (get-scored state :corp 0)]
      (click-card state :runner icew)
      (run-on state :hq)
      (core/update! state :corp (assoc-in (refresh yagi) [:counter :agenda] 1))
      (card-ability state :corp (refresh yagi) 0)
      (click-card state :corp icew)
      (click-card state :corp "Enigma")
      (is (nil? (:icon (find-card "Ice Wall" (:hand (get-corp)))))))))
