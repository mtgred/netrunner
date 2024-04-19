(ns game.cards.resources-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]
   [jinteki.validator :refer [legal?]]))

(deftest activist-support
  ;; Activist Support - Take tag if you have none; Corp gains bad pub if they have none
  (do-game
    (new-game {:runner {:deck ["Activist Support"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Activist Support")
    (is (zero? (count-tags state)))
    (take-credits state :runner)
    (is (= 1 (count-tags state)) "Runner took 1 tag; had none")
    (is (zero? (count-bad-pub state)))
    (take-credits state :corp)
    (is (= 1 (count-bad-pub state)) "Corp took 1 bad pub; had none")
    (take-credits state :runner)
    (is (= 1 (count-tags state)) "Runner had 1 tag; didn't take another")
    (take-credits state :corp)
    (is (= 1 (count-bad-pub state)) "Corp had 1 bad pub; didn't take another")))

(deftest adjusted-chronotype
  ;; Adjusted Chronotype - Ensure adjusted chronotype gains only 1 click when 2 clicks are lost
  (do-game
      (new-game {:runner {:deck ["Adjusted Chronotype" (qty "Beach Party" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:click (get-runner))) "Should have lost 2 clicks and gained 1 click")))

(deftest adjusted-chronotype-chronotype-to-cancel-out-mca-click-loss
    ;; Chronotype to cancel out MCA click loss
    (do-game
      (new-game {:corp {:deck ["MCA Austerity Policy"]}
                 :runner {:deck ["Adjusted Chronotype"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (take-credits state :runner)
      (play-from-hand state :corp "MCA Austerity Policy" "New remote")
      (let [mca (get-content state :remote1 0)]
        (rez state :corp mca)
        (card-ability state :corp mca 0)
        (is (= 1 (get-counters (refresh mca) :power)))
        (take-credits state :corp)
        ; runner does not lose a click
        (is (= 4 (:click (get-runner)))))))

(deftest adjusted-chronotype-chronotype-doesn-t-trigger-from-riot-suppression
    ;; Chronotype doesn't trigger from Riot Suppression
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Thomas Haas"]} :runner {:deck ["Adjusted Chronotype"]}})
      (play-from-hand state :corp "Thomas Haas" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (play-from-hand state :runner "Adjusted Chronotype")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (click-prompt state :runner "Get 3 fewer [Click] on the next turn")
      (take-credits state :corp)
      (is (= 1 (:click (get-runner))) "Runner still has 3 fewer clicks following turn")))

(deftest adjusted-chronotype-ensure-adjusted-chronotype-gains-2-clicks-when-2-clicks-are-lost-and-gcs-is-installed
    ;; Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed
    (do-game
      (new-game {:runner {:deck ["Adjusted Chronotype"
                                 (qty "Beach Party" 3)
                                 "Gene Conditioning Shoppe"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :runner "Beach Party")
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Should have lost 2 clicks and gained 2 clicks")
      (play-from-hand state :runner "Beach Party")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 3 (:click (get-runner))) "Should have lost 3 clicks and gained 2 clicks")))

(deftest adjusted-chronotype-doesn-t-gain-clicks-from-non-lose-actions-5119
    ;; Doesn't gain clicks from non 'lose' actions #5119
    (do-game
      (new-game {:runner {:deck ["Adjusted Chronotype" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (is (= 3 (:click (get-runner))) "Should have lost 1 click and gained 0 clicks")))

(deftest aeneas-informant
    ;; Aeneas Informant
    (do-game
      (new-game {:runner {:hand ["Aeneas Informant"]}
                 :corp {:hand ["Rashida Jaheem" "Hedge Fund"]}})
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Aeneas Informant")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No Aeneas Informant prompt")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (changed? [(:credit (get-runner)) 1]
            (click-prompt state :runner "Yes"))
          "Runner got 1 credit from Aeneas Informant")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (no-prompt? state :runner) "No Aeneas Informant prompt")))

(deftest aeneas-informant-does-not-trigger-on-gagarin-issue-#6392
  ;; Aeneas Informant & Gagarin
  (do-game
    (new-game {:runner {:hand ["Aeneas Informant" "Theophilius Bagbiter"]}
               :corp {:hand ["Rashida Jaheem"] :id "Gagarin Deep Space: Expanding the Horizon"}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (core/gain-clicks state :runner 1)
    (play-from-hand state :runner "Aeneas Informant")
    (is (changed? [(:credit (get-runner)) 0]
          (run-empty-server state "Server 1")
          (click-prompt state :runner "No action")
          (is (no-prompt? state :runner) "No prompt to use informant")
          (is (not (:run @state)) "Run over"))
        "did not spent or lose any credits")
    ;; using autoresolve
    (let [informant (get-resource state 0)]
      (card-ability state :runner informant 0)
      (click-prompt state :runner "Always"))
    (is (changed? [(:credit (get-runner)) 0]
          (run-empty-server state "Server 1")
          (click-prompt state :runner "No action"))
        "did not spent or lose any credits with autoresolve on")
    ;;can't pay (instead of refusing)
    (play-from-hand state :runner "Theophilius Bagbiter")
    (is (zero? (:credit (get-runner))))
    (is (changed? [(:credit (get-runner)) 0]
          (run-empty-server state "Server 1")
          (click-prompt state :runner "OK"))
        "did not spent or lose any credits with autoresolve on")
    ))

(deftest aeneas-informant-triggers-on-cards-moved-to-rfg
    ;; Aeneas Informant & Salsette Slums - Runner gains credits from cards moved to RFG
    (do-game
      (new-game {:runner {:hand ["Aeneas Informant" "Salsette Slums"]}
                 :corp {:hand ["Rashida Jaheem"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aeneas Informant")
      (play-from-hand state :runner "Salsette Slums")
      (run-empty-server state :hq)
      (click-prompt state :runner "[Salsette Slums] Remove card from game")
      (is (= "Rashida Jaheem" (:title (first (:rfg (get-corp))))) "Rashida Jaheem removed from game")
      (is (changed? [(:credit (get-runner)) 1]
            (click-prompt state :runner "Yes"))
          "Runner got 1 credit from Aeneas Informant")))

;; Aesop's Pawnshop
(deftest aesop-s-pawnshop-manual-use
  ;; Manual use
  (do-game
   (new-game {:runner {:deck ["Aesop's Pawnshop" "Cache"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Aesop's Pawnshop")
   (play-from-hand state :runner "Cache")
   (let [orig-credits (:credit (get-runner))
         ap (get-resource state 0)
         cache (get-program state 0)]
     (card-ability state :runner ap 0)
     (click-card state :runner cache)
     (card-ability state :runner ap 0)
     (is (not (prompt-is-type? state :runner :select)) "Aesop's has already been used this turn")
     (let [ap (get-resource state 0)
           cache (get-in @state [:runner :discard 0])]
       (is (= (+ 3 orig-credits) (:credit (get-runner))) "Should have only gained 3 credits")
       (is (not= cache nil) "Cache should be in Heap")
       (is (not= ap nil) "Aesops should still be installed")))))

(deftest aesop-s-pawnshop-triggered-at-start-of-turn
  ;; Triggered at start of turn
  (do-game
   (new-game {:runner {:deck ["Aesop's Pawnshop" "Daily Casts"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Aesop's Pawnshop")
   (play-from-hand state :runner "Daily Casts")
   (take-credits state :runner)
   (take-credits state :corp)
   (end-phase-12 state :runner)
   (let [dc (get-resource state 1)]
     (is (changed? [(:credit (get-runner)) 3]
           (click-prompt state :runner "Aesop's Pawnshop")
           (click-card state :runner dc))
         "Aesop's sells Daily Casts before it triggers so only 3 credits gained")
     (is (= (refresh dc) nil) "Daily Casts should be in Heap"))))

(deftest all-nighter
  ;; All-nighter - Click/trash to gain 2 clicks
  (do-game
    (new-game {:runner {:deck ["All-nighter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "All-nighter")
    (is (= 3 (:click (get-runner))))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 4 (:click (get-runner))) "Spent 1 click; gained 2 clicks")
    (is (= 1 (count (:discard (get-runner)))) "All-nighter is trashed")))

(deftest arruaceiras-crew
  (do-game
    (new-game {:corp {:hand ["Palisade" "Whitespace"]}
               :runner {:hand ["Arruaceiras Crew"]}})
    (play-from-hand state :corp "Palisade" "New remote")
    (play-from-hand state :corp "Whitespace" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Arruaceiras Crew")
    (let [pali (get-ice state :remote1 0)
          ac (get-resource state 0)]
      (run-on state "Server 1")
      (rez state :corp pali)
      (run-continue state)
      (card-ability state :runner (refresh ac) 1)
      (is (not (nil? (refresh pali))) "Palisade strength greater than 0, should not be trashed")
      (is (not (nil? (refresh ac))) "Arruaceiras Crew should not be trashed")
      (is (changed? [(get-strength (refresh pali)) -2
                     (count-tags state) 1]
                    (card-ability state :runner (refresh ac) 0))
          "Arruaceiras Crew lowers Palisade strength by 2")
      (fire-subs state (refresh pali)))
    (let [ws (get-ice state :remote2 0)
          ac (get-resource state 0)]
      (run-on state "Server 2")
      (rez state :corp ws)
      (run-continue state)
      (is (changed? [(get-strength (refresh ws)) 0
                     (count-tags state) 0]
                    (card-ability state :runner (refresh ac) 0))
          "Arruaceiras Crew first ability is once per turn")
      (card-ability state :runner (refresh ac) 1)
      (is (nil? (refresh ws)) "Whitespace should be trashed")
      (is (nil? (refresh ac)) "Arruaceiras Crew should be trashed"))))

(deftest artist-colony
  ;; Artist Colony
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Vanity Project"]}
               :runner {:deck ["Monolith" "Chatterjee University"]
                        :hand ["Artist Colony"]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner "Artist Colony")
    (card-ability state :runner (get-resource state 0) 0)
    (click-prompt state :runner "Chatterjee University")
    (click-card state :runner "Vanity Project")
    (is (last-n-log-contains? state 1 "forfeits 1 agenda .* to use Artist Colony to install Chatterjee University"))
    (is (= "Chatterjee University" (:title (get-resource state 1))))
    (is (empty? (:scored (get-runner))))))

(deftest asmund-pudlat
  ;; Asmund Pudlat
  (do-game
    (new-game {:runner {:hand ["Asmund Pudlat"]
                        :deck ["Unregistered S&W '35" (qty "Fermenter" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Asmund Pudlat")
    (is (changed? [(count (:choices (prompt-map :runner))) -1]
          (click-prompt state :runner "Fermenter"))
        "Runner cannot choose Fermenter twice")
    (click-prompt state :runner "Unregistered S&W '35")
    (let [asmund (get-resource state 0)]
      (is (= 2 (count (:hosted (refresh asmund)))) "Asmund Pudlat is hosting 2 cards")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (changed? [(count (:hosted (refresh asmund))) -1]
            (click-card state :runner (find-card "Unregistered S&W '35" (:hosted (refresh asmund)))))
          "Unregistered S&W '35 not hosted anymore")
      (is (= 1 (count (:hand (get-runner)))) "Card was added to grip")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (changed? [(count (:hosted (refresh asmund))) -1]
            (click-card state :runner (find-card "Fermenter" (:hosted (refresh asmund)))))
          "Fermenter not hosted anymore")
      (is (= 2 (count (:hand (get-runner)))) "Card was added to grip")
      (is (= 1 (count (:discard (get-runner)))) "Asmund Pudlat was trashed"))))

(deftest amelia-earhart
  (do-game
    (new-game {:runner {:hand ["Amelia Earhart" "The Maker's Eye" (qty "Legwork" 2)]
                        :credits 10}
               :corp {:hand [(qty "Hedge Fund" 3)]
                      :deck [(qty "Hedge Fund" 10)]
                      :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Amelia Earhart")
    (let [amelia (get-resource state 0)]
      (dotimes [_ 2]
        (is (changed? [(get-counters (refresh amelia) :power) 1]
                      (play-run-event state "Legwork" :hq)
                      (dotimes [_ 3]
                        (click-prompt state :runner "No action")))
          "Amelia Earhart gained 1 power counter"))
      (is (changed? [(get-counters (refresh amelia) :power) 1]
                      (play-run-event state "The Maker's Eye" :rd)
                      (dotimes [_ 3]
                        (click-prompt state :runner "No action")))
          "Amelia Earhart gained 1 power counter")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (:runner-phase-12 @state) "Runner in Step 1.2")
      (end-phase-12 state :runner)
      (is (changed? [(:credit (get-corp)) -10
                     (count (:discard (get-runner))) 1]
                    (click-prompt state :runner "Yes"))
          "Corp lost 10 credits and Amelia Earhart trashed"))))

(deftest asmund-pudlat-nothing-to-install-trashes-asmund-next-turn
  (do-game
    (new-game {:runner {:hand ["Asmund Pudlat"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Asmund Pudlat")
    (click-prompt state :runner "Cancel")
    (take-credits state :corp)
    (click-prompt state :runner "Done")
    (is (= 1 (count (:discard (get-runner)))) "Asmund Pudlat was trashed")))

(deftest avgustina-ivanovskaya
  ;; First time each turn you install a virus program, resist 1
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 5)]}
               :runner {:hand [(qty "Cache" 3) "Avgustina Ivanovskaya"] :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Avgustina Ivanovskaya")
    (play-from-hand state :runner "Cache")
    (is (no-prompt? state :corp) "No promp to resist")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (is (last-log-contains? state "uses Avgustina Ivanovskaya to sabotage 1") "Sabotage happened")))

(deftest backstitching
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Backstitching"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (core/set-mark state :hq)
    (play-from-hand state :runner "Backstitching")
    (run-on state :hq)
    (run-continue state)
    (click-prompt state :runner "Yes")
    (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")))

(deftest baklan-bochkin-gaining-power-counters-each-run
    ;; Gaining power counters each run.
    (do-game
      (new-game {:corp {:deck ["Vanilla" "Vanilla"]}
                 :runner {:deck ["\"Baklan\" Bochkin"]}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "\"Baklan\" Bochkin")
      (let [bak (get-resource state 0)
            van0 (get-ice state :hq 0)
            van1 (get-ice state :hq 1)]
        (run-on state "HQ")
        (run-continue-until state :success)
        (is (= 0 (get-counters (refresh bak) :power)) "No encounter so counter on Baklan yet")
        (run-on state "HQ")
        (rez state :corp van1)
        (run-continue state)
        (is (= 1 (get-counters (refresh bak) :power)) "There was an encounter, so counter on Baklan")
        (run-continue state)
        (run-jack-out state)
        (run-on state "HQ")
        (rez state :corp van0)
        (run-continue-until state :encounter-ice van1)
        (run-continue-until state :encounter-ice van0)
        (run-continue state)
        (run-jack-out state)
        (is (= 2 (get-counters (refresh bak) :power)) "Works on every run, but not every encounter"))))

(deftest baklan-bochkin-derezzing-current-ice
    ;; Derezzing current ice
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck ["\"Baklan\" Bochkin"]}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "\"Baklan\" Bochkin")
      (let [bak (get-resource state 0)
            van0 (get-ice state :hq 0)]
        (run-on state "HQ")
        (rez state :corp van0)
        (run-continue state)
        (is (rezzed? (refresh van0)) "Rezzed Vanilla")
        (card-ability state :runner bak 0)
        (is (not (rezzed? (refresh van0))) "Derezzed Vanilla")
        (is (nil? (refresh bak)) "Baklan is trashed")
        (is (= 1 (count-tags state)) "Got a tag"))))

(deftest baklan-bochkin-can-t-derez-current-ice-if-strength-is-too-high
    ;; Can't derez current ice if strength is too high
    (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["\"Baklan\" Bochkin"]}})
      (play-from-hand state :corp "Fire Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "\"Baklan\" Bochkin")
      (let [bak (get-resource state 0)
            fw (get-ice state :hq 0)]
        (run-on state "HQ")
        (rez state :corp fw)
        (run-continue state)
        (is (rezzed? (refresh fw)) "Rezzed Fire Wall")
        (card-ability state :runner bak 0)
        (is (rezzed? (refresh fw)) "Fire Wall not derezzed as too strong")
        (is (zero? (count-tags state)) "Got a tag"))))

(deftest bank-job-manhunt-trace-happens-first
    ;; Manhunt trace happens first
    (do-game
      (new-game {:corp {:deck ["Manhunt" "PAD Campaign"]}
                 :runner {:deck ["Bank Job"]}})
      (play-from-hand state :corp "Manhunt")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "2") ; Manhunt trace active
      (click-prompt state :runner "0")
      (click-prompt state :runner "Bank Job")
      (is (= "Bank Job" (:title (:card (prompt-map :runner))))
          "Bank Job prompt active")
      (click-prompt state :runner "8")
      (is (empty? (get-resource state)) "Bank Job trashed after all credits taken")
      (is (= 1 (count (:discard (get-runner)))))))

(deftest bank-job-choose-which-to-use-when-2-copies-are-installed
    ;; Choose which to use when 2+ copies are installed
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck [(qty "Bank Job" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Bank Job")
      (click-prompt state :runner "4")
      (play-from-hand state :runner "Bank Job")
      (let [bj1 (get-resource state 0)
            bj2 (get-resource state 1)]
        (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Bank Job")
        (click-card state :runner bj2)
        (click-prompt state :runner "6")
        (is (= 13 (:credit (get-runner))))
        (is (= 2 (get-counters (refresh bj2) :credit)) "2 credits remaining on 2nd copy"))))

(deftest bank-job-trashing-when-empty-with-multiple-installed-bank-jobs
    ;; Trashing when empty with multiple installed Bank Jobs
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck [(qty "Bank Job" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bank Job")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Bank Job")
      (click-prompt state :runner "4")
      (play-from-hand state :runner "Bank Job")
      (let [bj1 (get-resource state 0)
            bj2 (get-resource state 1)]
        (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Bank Job")
        (click-card state :runner bj1)
        (is (changed? [(:credit (get-runner)) 4]
              (click-prompt state :runner "4"))
            "Got last 4 credits from Bank Job")
        (is (nil? (refresh bj1)) "Bank Job 1 got moved to the heap")
        (is (some? bj2) "Bank Job 2 still installed")
        (is (= 1 (count (:discard (get-runner)))) "One Bank Job moved to heap")
        (is (= 8 (get-counters (refresh bj2) :credit)) "8 credits remaining on 2nd copy"))))

(deftest bank-job-you-can-choose-between-security-testing-and-bank-job
    ;; You can choose between Security Testing and Bank Job
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Bank Job" "Security Testing"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Bank Job")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Server 1")
      (is (= 6 (:credit (get-runner))))
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Security Testing")
      (is (no-prompt? state :runner) "No Bank Job replacement choice")
      (is (= 8 (:credit (get-runner))) "Security Testing paid 2c")))

(deftest bazaar
  ;; Bazaar - Only triggers when installing from Grip
  (do-game
      (new-game {:runner {:deck ["Street Peddler" "Bazaar"
                                (qty "Spy Camera" 6)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Bazaar" "Spy Camera" "Spy Camera" "Spy Camera"])
      (play-from-hand state :runner "Bazaar")
      (play-from-hand state :runner "Street Peddler")
      (let [peddler (get-resource state 1)]
        (card-ability state :runner peddler 0)
        (click-prompt state :runner (first (:hosted peddler)))
        (is (no-prompt? state :runner) "No Bazaar prompt from install off Peddler"))))

(deftest bazaar-bazaar-with-kate-test
    ;; bazaar with kate test
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Bazaar" "Cache" (qty "Clone Chip" 6)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Bazaar" "Cache" "Clone Chip" "Clone Chip" "Clone Chip"])
      (play-from-hand state :runner "Bazaar")
      (play-from-hand state :runner "Clone Chip")
      (click-prompt state :runner "Yes")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits (-1 Bazaar, -1 second clone chip")))

(deftest bazaar-bazaar-with-az-test
    ;; bazaar with Az test
    (do-game
      (new-game {:runner {:id "Az McCaffrey: Mechanical Prodigy"
                          :deck ["Bazaar" (qty "Clone Chip" 6)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Bazaar" "Clone Chip" "Clone Chip" "Clone Chip"])
      (play-from-hand state :runner "Bazaar")
      (play-from-hand state :runner "Clone Chip")
      (click-prompt state :runner "Yes")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits (-1 Bazaar, -1 second clone chip")))

(deftest bazaar-with-replicator-4456
    ;; with replicator #4456
    (do-game
      (new-game {:runner {:deck [(qty "Clone Chip" 5)]
                          :hand ["Bazaar" "In the Groove" (qty "Replicator" 3)]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "In the Groove")
      (play-from-hand state :runner "Bazaar")
      (is (= "Choose one" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Draw 1 card")
      (play-from-hand state :runner "Replicator")
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :runner))))
      (click-prompt state :runner "In the Groove")
      (is (= "Choose one" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Draw 1 card")
      (is (= "Install another copy of Replicator?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :runner))))
      (click-prompt state :runner "In the Groove")
      (is (= "Choose one" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Draw 1 card")
      (is (= "Install another copy of Replicator?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Draw 1 card")
      (is (no-prompt? state :corp))
      (is (no-prompt? state :runner))))

(deftest beach-party
  ;; Beach Party - Lose 1 click when turn begins; hand size increased by 5
  (do-game
    (new-game {:runner {:deck ["Beach Party"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Beach Party")
    (is (= 10 (hand-size :runner)) "Max hand size increased by 5")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 3 (:click (get-runner))) "Lost 1 click at turn start")))

(deftest beatriz-friere-gonzalez
  (do-game
    (new-game {:runner {:deck ["Beatriz Friere Gonzalez"]}
               :corp {:hand ["Hedge Fund"]
                      :deck [(qty "Rashida Jaheem" 5)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Beatriz Friere Gonzalez")
    (is (changed? [(:click (get-runner)) -2]
          (card-ability state :runner (get-resource state 0) 0))
        "Spent 2 clicks")
    (run-continue state)
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (no-prompt? state :runner) "No more accesses")))

(deftest bhagat-only-trigger-on-first-run
    ;; only trigger on first run
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3) (qty "Architect" 3)]
                        :hand ["AR-Enhanced Security"]}
                :runner {:deck ["Bhagat"]}})
      (core/gain state :corp :click 10 :credit 10)
      (play-and-score state "AR-Enhanced Security")
      (take-credits state :corp)
      (play-from-hand state :runner "Bhagat")
      (run-empty-server state :hq)
      (is (= 0 (count-tags state)) "Runner has no tag")
      (is (= 1 (count (:discard (get-corp)))) "Bhagat milled one card")))

(deftest bhagat-doesn-t-trigger-on-second-successful-run
    ;; only trigger on first run
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3) (qty "Architect" 3)]}
                :runner {:deck ["Bhagat"]}})
      (starting-hand state :corp [])
      (take-credits state :corp)
      (run-empty-server state :hq)
      (play-from-hand state :runner "Bhagat")
      (run-empty-server state :hq)
      (is (empty? (:discard (get-corp))) "Bhagat did not trigger on second successful run")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (is (= 1 (count (:discard (get-corp)))) "Bhagat milled one card")))

;; Bloo Moose
(deftest bloo-moose-manual-use
  ;; Manual use
  (do-game
   (new-game {:runner {:deck ["Bloo Moose"] :discard ["Sure Gamble"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Bloo Moose")
        (let [bm (get-resource state 0)]
     (card-ability state :runner bm 0)
     (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
     (is (zero? (count (:discard (get-runner)))) "0 cards in discard")
     (is (= 1 (count (:rfg (get-runner)))) "1 card in rfg")
     (card-ability state :runner bm 0)
     (is (not (prompt-is-type? state :runner :select)) "Bloo Moose has already been used this turn"))))

(deftest bloo-moose-triggered-at-start-of-turn
  ;; Triggered at start of turn
  (do-game
    (new-game {:runner {:deck ["Bloo Moose"] :discard ["Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Bloo Moose")
    (take-credits state :runner)
    (take-credits state :corp)
    (end-phase-12 state :runner)
    (is (changed? [(:credit (get-runner)) 2]
          (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner)))))
        "Remove 1 card from the game and gain 2 credits")
    (is (zero? (count (:discard (get-runner)))) "0 cards in discard")
    (is (= 1 (count (:rfg (get-runner)))) "1 card in rfg")))

(deftest charlatan
  ;; Charlatan
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Charlatan"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Charlatan")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "HQ")
      (click-prompt state :runner "Yes")
      (run-continue state)
      (is (second-last-log-contains? state "Runner bypasses Ice Wall."))))

(deftest charlatan-only-works-on-first-rezzed-ice
      ;; Only works on first rezzed ice
    (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand [(qty "Ice Wall" 2)]}
                   :runner {:hand ["Charlatan"]
                            :credits 10}})
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Ice Wall" "HQ")
        (rez state :corp (get-ice state :hq 0))
        (rez state :corp (get-ice state :hq 1))
        (take-credits state :corp)
        (play-from-hand state :runner "Charlatan")
        (card-ability state :runner (get-resource state 0) 0)
        (click-prompt state :runner "HQ")
        (click-prompt state :runner "Yes")
        (run-continue state)
        (is (no-prompt? state :runner))))

(deftest charlatan-only-works-if-ice-is-already-rezzed
    ;; Only works if ice is already rezzed
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Charlatan"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Charlatan")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "HQ")
      (rez state :corp (get-ice state :hq 0))
      (is (no-prompt? state :runner))
      (run-continue state)
      (is (last-log-contains? state "Runner encounters Ice Wall protecting HQ at position 0."))))

(deftest chrome-parlor
  ;; Chrome Parlor - Prevent all meat/brain dmg when installing cybernetics
  (do-game
    (new-game {:corp {:deck ["Traffic Accident"]}
               :runner {:deck ["Chrome Parlor" "Titanium Ribs"
                               "Brain Cage" (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Chrome Parlor")
    (play-from-hand state :runner "Titanium Ribs")
    (is (no-prompt? state :runner) "Damage prevented, no Ribs prompt to choose cards")
    (is (= 3 (count (:hand (get-runner)))))
    (play-from-hand state :runner "Brain Cage")
    (is (= 2 (count (:hand (get-runner)))) "No cards lost")
    (is (zero? (:brain-damage (get-runner))))
    (is (= 8 (hand-size :runner)) "Runner hand size boosted by Brain Cage")
    (take-credits state :runner)
    (gain-tags state :runner 2)
    (trash state :runner (get-hardware state 0))
    (play-from-hand state :corp "Traffic Accident")
    (is (= 3 (count (:discard (get-runner)))) "Conventional meat damage not prevented by Parlor")))

(deftest citadel-sanctuary
  ;; Citadel Sanctuary
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Scorched Earth"]}
                 :runner {:deck ["Citadel Sanctuary" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Citadel Sanctuary")
      (take-credits state :runner)
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:discard (get-runner)))) "No cards have been discarded or trashed yet")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 3 (count (:discard (get-runner)))) "CS and all cards in grip are trashed")))

(deftest citadel-sanctuary-end-of-turn-trace
    ;; End of turn trace
    (do-game
      (new-game {:runner {:hand ["Citadel Sanctuary"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Citadel Sanctuary")
      (take-credits state :runner)
      (is (no-prompt? state :runner) "No Runner prompt because not tagged")
      (is (no-prompt? state :corp) "No Corp prompt because runner not tagged")
      (take-credits state :corp)
      (gain-tags state :runner 2)
      (end-turn state :runner)
      (is (= "Waiting for Corp to boost trace strength" (:msg (prompt-map :runner))) "Runner has trace waiting prompt")
      (is (= "Boost trace strength?" (:msg (prompt-map :corp))) "Corp has trace prompt")
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      (is (= 1 (count-tags state)) "Tag removed")))

(deftest citadel-sanctuary-interaction-with-corporate-grant-and-thunder-art-gallery
    ;; Interaction with Corporate Grant and Thunder Art Gallery
    (do-game
      (new-game {:runner {:deck ["Citadel Sanctuary" "Thunder Art Gallery" "Corroder" "Corporate \"Grant\""]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Citadel Sanctuary")
      (play-from-hand state :runner "Thunder Art Gallery")
      (play-from-hand state :runner "Corporate \"Grant\"")
      (take-credits state :runner)
      (take-credits state :corp)
      (gain-tags state :runner 1)
      (core/lose state :runner :click 3)
      (end-turn state :runner)
      (is (= 11 (:credit (get-corp))) "Corp has 11 credits before Corporate Grant")
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      (is (not (:end-turn @state)) "Runner turn has not yet ended")
      (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
      (is (:end-turn @state) "Runner turn has now ended")
      (is (= 10 (:credit (get-corp))) "Corp lost 1 credit to Corporate Grant")))

(deftest climactic-showdown-no-eligible-servers
    ;; No eligible servers
    (do-game
      (new-game {:runner {:deck ["Climactic Showdown"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Climactic Showdown")
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= "Climactic Showdown" (-> (get-runner) :rfg first :title)) "Climactic Showdown RFGed")
      (is (no-prompt? state :runner) "No Runner prompt because no eligible servers")
      (is (no-prompt? state :corp) "No Corp prompt because no eligible servers")))

(deftest climactic-showdown-corp-trashes-ice
    ;; Corp trashes ice
    (do-game
      (new-game {:runner {:deck ["Climactic Showdown"]}
                 :corp {:deck [(qty "Kitsune" 5)]}})
      (play-from-hand state :corp "Kitsune" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Climactic Showdown")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= "Climactic Showdown" (-> (get-runner) :rfg first :title)) "Climactic Showdown RFGed")
      (click-prompt state :runner "HQ")
      (click-card state :corp (get-ice state :hq 0))
      (is (no-prompt? state :corp) "Corp trashed their ice and corp prompt is gone")
      (is (no-prompt? state :runner) "Corp trashed their ice and runner prompt is gone")
      (is (= "Kitsune" (-> (get-corp) :discard first :title)) "Kitsune trashed")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner done with run after 1 access")
      (is (not (:run @state)) "Run over")))

(deftest climactic-showdown-corp-doesn-t-trash-access-hq
    ;; Corp doesn't trash, access HQ
    (do-game
      (new-game {:runner {:deck ["Climactic Showdown"]}
                 :corp {:deck [(qty "Vanilla" 10)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/move state :corp (find-card "Vanilla" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "Climactic Showdown")
      (take-credits state :runner)
      (core/move state :corp (find-card "Vanilla" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (is (= "Climactic Showdown" (-> (get-runner) :rfg first :title)) "Climactic Showdown RFGed")
      (click-prompt state :runner "Archives")
      (click-prompt state :corp "Done")
      (is (no-prompt? state :corp) "Corp refused trash and corp prompt is gone")
      (is (no-prompt? state :runner) "Corp refused trash and runner prompt is gone")
      (is (empty? (:discard (get-corp))) "Nothing trashed")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner done with run after 3 accesses")
      (is (not (:run @state)) "3 access run over")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner done with 2nd run after 1 accesses")
      (is (not (:run @state)) "Single access run over")
      (take-credits state :runner)
      (core/move state :corp (find-card "Vanilla" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner done with second turns run after 1 access")
      (is (not (:run @state)) "2nd turn run over"))
    (testing "Corp doesn't trash, mid-run access"
      (do-game
        (new-game {:runner {:deck ["Climactic Showdown"]}
                   :corp {:deck [(qty "Kitsune" 10)]}})
        (play-from-hand state :corp "Kitsune" "R&D")
        (take-credits state :corp)
        (play-from-hand state :runner "Climactic Showdown")
        (take-credits state :runner)
        (core/move state :corp (find-card "Kitsune" (:hand (get-corp))) :deck)
        (take-credits state :corp)
        (is (= "Climactic Showdown" (-> (get-runner) :rfg first :title)) "Climactic Showdown RFGed")
        (click-prompt state :runner "R&D")
        (click-prompt state :corp "Done")
        (run-on state "R&D")
        (rez state :corp (get-ice state :rd 0))
        (run-continue state)
        (card-subroutine state :corp (get-ice state :rd 0) 0)
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Kitsune" (:hand (get-corp))))
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "Runner done with run after 3 accesses")
        (is (not (:run @state)) "Run over"))))

(deftest compromised-employee
  ;; Compromised Employee - Gain 1c every time Corp rezzes ice
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Snatch and Grab" (qty "Pup" 2) "Launch Campaign"]}
               :runner {:deck ["Compromised Employee"]}})
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Pup" "R&D")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Compromised Employee")
    (let [ce (get-resource state 0)]
      (is (= 1 (get-counters (refresh ce) :recurring)) "Has 1 recurring credit")
      (rez state :corp (get-ice state :hq 0))
      (is (= 4 (:credit (get-runner))) "Gained 1c from ice rez")
      (rez state :corp (get-ice state :rd 0))
      (is (= 5 (:credit (get-runner))) "Gained 1c from ice rez")
      (rez state :corp (get-content state :remote1 0))
      (is (= 5 (:credit (get-runner))) "Asset rezzed, no credit gained")
      (take-credits state :runner)
      (play-from-hand state :corp "Snatch and Grab")
      (click-prompt state :corp "0")
      (is (= (+ (:credit (get-runner)) (get-counters (refresh ce) :recurring))
             (:choices (prompt-map :runner))) "9 total available credits for the trace")
      (click-prompt state :runner "9")
      (dotimes [_ 1]
        (click-card state :runner ce))
      (is (zero? (get-counters (refresh ce) :recurring)) "Has used recurring credit"))))

(deftest cookbook
  ;; Cookbook - add a free virus counter to installed virus programs
  (do-game
      (new-game {:runner {:deck ["Cookbook" "Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cookbook")
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 2 (get-counters (refresh imp) :virus)) "Imp installed with two counters")
        (click-prompt state :runner "Yes")
        (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter"))))

(deftest cookbook-autoresolve
    ;; Autoresolve
    (do-game
      (new-game {:runner {:deck ["Cookbook" "Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cookbook")
      (let [cookbook (get-resource state 0)]
        (card-ability state :runner cookbook 0)
        (click-prompt state :runner "Always"))
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest cookbook-grimoire-and-cookbook-both-add-counters
    ;; Grimoire and Cookbook both add counters
    (do-game
     (new-game {:runner {:hand ["Grimoire" "Cookbook" "Leech" "Fermenter"]
                         :credits 10}})
     (take-credits state :corp)
     (play-from-hand state :runner "Grimoire")
     (play-from-hand state :runner "Cookbook")
     (play-from-hand state :runner "Leech")
     (click-prompt state :runner "Grimoire")
     (click-prompt state :runner "Yes")
     (is (= 2 (get-counters (get-program state 0) :virus)) "Leech has 2 counters from Grimoire and Cookbook")
     (card-ability state :runner (get-resource state 0) 0)
     (click-prompt state :runner "Always")
     (play-from-hand state :runner "Fermenter")
     (click-prompt state :runner "Grimoire")
     (click-prompt state :runner "Cookbook")
     (is (= 3 (get-counters (get-program state 1) :virus)) "Fermenter has 2 additional counters from Grimoire and Cookbook")))

(deftest councilman-rez-prevention
    ;; Rez prevention
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Chief Slee"]
                        :credits 10}
                 :runner {:deck ["Councilman"]}})
      (play-from-hand state :corp "Chief Slee" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Councilman")
      (let [slee (get-content state :remote1 0)]
        (rez state :corp slee)
        (is (changed? [(:credit (get-runner)) -2]
              ;; Runner triggers Councilman
              (click-prompt state :runner "Yes"))
            "Runner pays 2 credits to derez Slee")
        (is (not (rezzed? (refresh slee))) "Chief Slee no longer rezzed")
        (rez state :corp (refresh slee) {:expect-rez false})
        (is (not (rezzed? (refresh slee))) "Chief Slee cannot be rezzed")
        (take-credits state :runner)
        ;; Next turn
        (rez state :corp (refresh slee))
        (is (rezzed? (refresh slee)) "Chief Slee can be rezzed next turn"))))

(deftest councilman-rez-prevention-happens-before-draw-on-rez
    ;; Rez prevention happens before draw-on-rez
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Spin Doctor"]
                        :credits 10}
                 :runner {:deck ["Councilman"]}})
      (play-from-hand state :corp "Spin Doctor" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Councilman")
      (let [spin (get-content state :remote1 0)
            hand (:hand (get-corp))]
        (rez state :corp spin)
        (is (= (count hand) (count (:hand (get-corp)))) "Draw waits for abilities")
        (is (= "Trash Councilman and pay 0 [Credits] to derez Spin Doctor?"
               (:msg (prompt-map :runner))))
        (is (changed? [(:credit (get-runner)) 0]
              ;; Runner triggers Councilman
              (click-prompt state :runner "Yes"))
            "Runner pays 0 credits to derez Spin Doctor")
        (is (not (rezzed? (refresh spin))) "Spin Doctor no longer rezzed")
        (is (= (count hand) (count (:hand (get-corp)))))
        (rez state :corp (refresh spin) {:expect-rez false})
        (is (not (rezzed? (refresh spin))) "Spin Doctor cannot be rezzed")
        (is (= (count hand) (count (:hand (get-corp)))))
        (take-credits state :runner)
        ;; Next turn
        (is (changed? [(count (:hand (get-corp))) 2]
              (rez state :corp (refresh spin))
              (is (rezzed? (refresh spin)) "Spin Doctor can be rezzed next turn"))
            "Corp draws 2 cards"))))

(deftest councilman-rezz-no-longer-prevented-when-card-changes-zone-issues-1571
    ;; Rezz no longer prevented when card changes zone (issues #1571)
    (do-game
      (new-game {:corp {:deck ["Jackson Howard"]}
                 :runner {:deck ["Councilman"]}})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Councilman")
      (take-credits state :runner)
      (let [jhow (get-content state :remote1 0)]
        (rez state :corp jhow)
        (click-prompt state :runner "Yes")
        (is (not (rezzed? (refresh jhow))) "Jackson Howard no longer rezzed")
        (core/move state :corp (refresh jhow) :hand))
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (let [jhow (get-content state :remote2 0)]
        (rez state :corp jhow)
        (is (rezzed? (refresh jhow)) "Jackson Howard can be rezzed after changing zone"))))

(deftest councilman-preventing-councilman-s-self-trash-prevents-the-rez-prevention-effect
    ;; Preventing Councilman's self-trash prevents the rez prevention effect
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Chief Slee"]}
                 :runner {:deck ["Councilman" "Fall Guy"]}})
      (play-from-hand state :corp "Chief Slee" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Councilman")
      (play-from-hand state :runner "Fall Guy")
      (take-credits state :runner)
      (let [slee (get-content state :remote1 0)
            councilman (get-resource state 0)
            fall-guy (get-resource state 1)]
        (rez state :corp slee)
        (is (changed? [(:credit (get-runner)) -2]
              (click-prompt state :runner "Yes")
              (card-ability state :runner fall-guy 0)
              (is (rezzed? (refresh slee)) "Chief Slee still rezzed")
              (is (refresh councilman) "Councilman's trash is prevented"))
            "Runner still pays for Councilman effect"))))

(deftest counter-surveillance-trash-to-run-on-successful-run-access-cards-equal-to-tags-and-pay-that-amount-in-credits
    ;; Trash to run, on successful run access cards equal to Tags and pay that amount in credits
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
                 :runner {:deck ["Counter Surveillance"]}})
      (take-credits state :corp)
      (gain-tags state :runner 2)
      (play-from-hand state :runner "Counter Surveillance")
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (let [cs (get-resource state 0)]
        (card-ability state :runner cs 0)
        (click-prompt state :runner "HQ")
        (run-continue state)
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (click-prompt state :runner "2")
        (is (accessing state "Hedge Fund"))
        (click-prompt state :runner "No action")
        (is (accessing state "Hedge Fund"))
        (click-prompt state :runner "No action")
        (is (= 1 (count (:discard (get-runner)))) "Counter Surveillance trashed")
        (is (= 2 (:credit (get-runner))) "Runner has 2 credits"))))

(deftest counter-surveillance-test-obelus-does-not-trigger-before-counter-surveillance-accesses-are-done-issues-2675
    ;; Test Obelus does not trigger before Counter Surveillance accesses are done. Issues #2675
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
                 :runner {:deck ["Counter Surveillance" "Obelus" (qty "Sure Gamble" 3)]}})
      (starting-hand state :runner ["Counter Surveillance" "Obelus"])
      (take-credits state :corp)
      (gain-tags state :runner 2)
      (core/gain state :runner :credit 2)
      (is (= 7 (:credit (get-runner))) "Runner has 7 credits")
      (play-from-hand state :runner "Counter Surveillance")
      (play-from-hand state :runner "Obelus")
      (is (= 2 (:credit (get-runner))) "Runner has 2 credits") ; Runner has enough credits to pay for CS
      (let [cs (get-resource state 0)]
        (card-ability state :runner cs 0)
        (click-prompt state :runner "HQ")
        (run-continue state)
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (zero? (count (:hand (get-runner)))) "Runner did not draw cards from Obelus yet")
        (click-prompt state :runner "2")
        (is (accessing state "Hedge Fund"))
        (is (zero? (count (:hand (get-runner)))) "Runner did not draw cards from Obelus yet")
        (click-prompt state :runner "No action")
        (is (accessing state "Hedge Fund"))
        (click-prompt state :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Runner did draw cards from Obelus after all accesses are done")
        (is (= 1 (count (:discard (get-runner)))) "Counter Surveillance trashed")
        (is (zero? (:credit (get-runner))) "Runner has no credits"))))

(deftest counter-surveillance-interaction-with-by-any-means-issue-3377
    ;; Interaction with By Any Means. Issue #3377
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Blue Level Clearance" "Red Level Clearance"]}
                 :runner {:hand [(qty "Sure Gamble" 2) "Counter Surveillance" "By Any Means"]
                          :tags 2}})
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (play-from-hand state :runner "Counter Surveillance")
      (is (= 2 (:credit (get-runner))) "Runner has 4 credits")
      (let [cs (get-resource state 0)]
        (card-ability state :runner cs 0)
        (click-prompt state :runner "HQ")
        (run-continue state)
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (click-prompt state :runner "2")
        (is (second-last-log-contains? state "Runner uses By Any Means to trash"))
        (is (second-last-log-contains? state "Runner uses By Any Means to trash"))
        (is (= 4 (count (:discard (get-runner)))) "Counter Surveillance trashed")
        (is (zero? (:credit (get-runner))) "Runner has 2 credits"))))

(deftest crash-space-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Crash Space"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Crash Space")
      (gain-tags state :corp 1)
      (let [cs (get-resource state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (remove-tag state :runner)
              (click-card state :runner cs)
              (click-card state :runner cs))
            "Used 2 credit from Crash Space"))))

(deftest crowdfunding-credit-gain-behavior
    ;; Credit gain behavior
    (do-game
      (new-game {:runner {:deck ["Crowdfunding" "Sure Gamble"]}})
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "Crowdfunding")
      (let [cf (get-resource state 0)]
        ;; Number of credits
        (is (= 3 (get-counters cf :credit)))
        (is (= 5 (:credit (get-runner))))
        ;; End turn
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh cf) :credit)))
        (is (= 9 (:credit (get-runner))))
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh cf) :credit)))
        (is (= 14 (:credit (get-runner))))
        (is (= 1 (count (:deck (get-runner)))) "1 card in deck")
        (is (empty? (:hand (get-runner))) "No cards in hand")
        (is (empty? (:discard (get-runner))) "No cards in discard")
        (take-credits state :runner)
        (take-credits state :corp)
        (end-phase-12 state :runner)
        (is (= 19 (:credit (get-runner))))
        (is (empty? (:deck (get-runner))) "No cards in deck")
        (is (= 1 (count (:hand (get-runner)))) "1 card in hand")
        (is (= 1 (count (:discard (get-runner)))) "1 card in discard")
        (is (nil? (get-resource state 0)) "Crowdfunding not installed"))))

(deftest crowdfunding-install-from-heap
    ;; Install from heap
    (do-game
      (new-game {:runner {:discard ["Crowdfunding"]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (is (no-prompt? state :runner) "No install prompt if no runs")
      (is (= 1 (count (:discard (get-runner)))) "1 card in discard")
      (is (empty? (get-resource state)) "Crowdfunding not installed")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (run-empty-server state :archives)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (click-prompt state :runner "Yes")
      (is (empty? (:discard (get-runner))) "Crowdfunding not in discard")
      (is (= 1 (count (get-resource state))) "Crowdfunding reinstalled")))

(deftest crowdfunding-ignores-costs-from-cards-like-scarcity-of-resources-3924
    ;; Ignores costs from cards like Scarcity of Resources (#3924)
    (do-game
      (new-game {:corp {:hand ["Scarcity of Resources"]
                        :deck [(qty "Hedge Fund" 5)]}
                 :runner {:discard ["Crowdfunding"]}})
      (play-from-hand state :corp "Scarcity of Resources")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (run-empty-server state :archives)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (let [credits (:credit (get-runner))]
        (click-prompt state :runner "Yes")
        (is (= credits (:credit (get-runner))) "Installing Crowdfunding should cost 0"))))

(deftest crowdfunding-should-only-prompt-on-successful-runs-3926
      ;; Should only prompt on successful runs (#3926)
    (do-game
        (new-game {:corp {:hand ["Ice Wall"]
                          :deck [(qty "Hedge Fund" 5)]}
                   :runner {:discard ["Crowdfunding"]}})
        (play-from-hand state :corp "Ice Wall" "HQ")
        (take-credits state :corp)
        (let [iw (get-ice state :hq 0)]
          (run-on state :hq)
          (rez state :corp iw)
          (run-continue state)
          (card-subroutine state :corp iw 0)
          (run-on state :hq)
          (run-continue state)
          (card-subroutine state :corp iw 0)
          (run-empty-server state :archives)
          (run-empty-server state :archives)
          (take-credits state :runner)
          (is (no-prompt? state :runner) "Crowdfunding shouldn't prompt for install"))))

(deftest crowdfunding-heap-locked-test
    ;; Heap locked test
    (do-game
      (new-game {:corp {:hand ["Blacklist"]}
                 :runner {:discard ["Crowdfunding"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (no-prompt? state :runner) "No install prompt if no runs")
      (is (= 1 (count (:discard (get-runner)))) "1 card in discard")
      (is (empty? (get-resource state)) "Crowdfunding not installed")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (run-empty-server state :archives)
      (run-empty-server state :archives)
      (take-credits state :runner)
      (is (no-prompt? state :runner) "No install prompt when heap is locked")
      (is (seq (:discard (get-runner))) "Crowdfunding is in discard")
      (is (zero? (count (get-resource state))) "Crowdfunding not installed")))

(deftest crypt-gains-counters-when-running-archives
    ;; Gains counters when running Archives
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:hand ["Crypt"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Crypt")
      (let [crypt (get-resource state 0)]
        (run-empty-server state "Archives")
        (is (= "Place 1 virus counter on Crypt?" (:msg (prompt-map :runner))))
        (click-prompt state :runner "Yes")
        (is (= 1 (get-counters (refresh crypt) :virus)))
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (not (:run @state)) "Run has ended")
        (is (no-prompt? state :runner) "No prompt for virus tokens")
        (is (= 1 (get-counters (refresh crypt) :virus))))))

(deftest crypt-ability-can-install-a-virus-card
    ;; Ability can install a virus card
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Crypsis" "Djinn"]
                          :hand ["Crypt"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Crypt")
      (let [crypt (get-resource state 0)]
        (core/add-counter state :runner (refresh crypt) :virus 2)
        (card-ability state :runner (refresh crypt) 0)
        (is (no-prompt? state :runner) "Crypt ability costs 3 virus counters")
        (core/add-counter state :runner (refresh crypt) :virus 1)
        (is (changed? [(:click (get-runner)) -1
               (:credit (get-runner)) -5]
              (card-ability state :runner (refresh crypt) 0)
              (is (= ["Crypsis" "Cancel"] (map #(or (:title %) %) (prompt-buttons :runner))))
              (click-prompt state :runner "Crypsis"))
            "Crypt ability costs 1 click")
        (is (= "Crypsis" (:title (get-program state 0))))
        (is (nil? (get-resource state 0)) "Crypt ability costs self-trash"))))

(deftest crypt-install-is-marked-as-ability-5058
    ;; Install is marked as ability #5058
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Crypsis"]
                          :hand ["Paladin Poemu" "Crypt"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Crypt")
      (play-from-hand state :runner "Paladin Poemu")
      (let [crypt (get-resource state 0)]
        (core/add-counter state :runner (refresh crypt) :virus 3)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (changed? [(:credit (get-runner)) -4]
              (card-ability state :runner (refresh crypt) 0)
              (click-prompt state :runner "Crypsis")
              (click-card state :runner "Paladin Poemu"))
            "Only pay 4 for Crypsis, using 1 from Paladin Poemu"))))

(deftest cybertrooper-talut
  ;; Cybertrooper Talut
  (do-game
     (new-game {:runner {:deck ["Cybertrooper Talut"
                                "Corroder"
                                "Aumakua"]}})
     (take-credits state :corp)
     (core/gain state :runner :credit 10)
     (is (= 0 (get-link state)) "Start with 0 link")
     (play-from-hand state :runner "Cybertrooper Talut")
     (is (= 1 (get-link state)) "Gained 1 link")
     (play-from-hand state :runner "Corroder")
     (play-from-hand state :runner "Aumakua")
     (let [cor (get-program state 0)
           aum (get-program state 1)]
       (is (= 4 (get-strength (refresh cor))) "+2 strength by Talut")
       (is (= 0 (get-strength (refresh aum))) "No strength boost for AI")
       (run-on state :hq)
       (card-ability state :runner cor 1)
       (is (= 5 (get-strength (refresh cor))) "+1 strength by hand")
       (run-jack-out state)
       (is (= 4 (get-strength (refresh cor))) "Strength back down to 4")
       (take-credits state :runner)
       (is (= 2 (get-strength (refresh cor))) "Corroder strength back down to normal"))))

(deftest dadiana-chacon-can-fire-mid-trace
    ;; Can fire mid-trace
    (do-game
      (new-game {:runner {:deck ["Dadiana Chacon"
                                 "Corroder"
                                 (qty "Sure Gamble" 3)]}
                 :corp {:deck ["SEA Source"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dadiana Chacon")
      (play-from-hand state :runner "Corroder") ;to get money
      (take-credits state :runner)
      (is (< (:credit (get-runner)) 6)
          "Chacon can trigger because runner has < 6 creds")
      (is (changed? [(:credit (get-runner)) 1]
            (take-credits state :corp)))
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (click-prompt state :corp "0")
      (is (changed? [(count (:hand (get-runner))) -3]
            (click-prompt state :runner (str (:credit (get-runner)))))
          "Spending all their money on trace causes Chacon to resolve")))

(deftest dadiana-chacon-ability-is-interactive
    ;; Lewi Guilherme and Dadiana Chacon should go along well
    (do-game
      (new-game {:runner {:hand ["Dadiana Chacon" "Lewi Guilherme" "Inti"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dadiana Chacon")
      (play-from-hand state :runner "Lewi Guilherme")
      (play-from-hand state :runner "Inti")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (changed? [(:credit (get-runner)) 0]
            (click-prompt state :runner "Lewi Guilherme")
            (click-prompt state :runner "Pay 1 [Credits]"))
          "Credit loss from Lewi triggers Dadiana")))

(deftest dadiana-chacon-when-playing-a-card
    ;; when playing a card
    (do-game
      (new-game {:runner {:hand ["Dadiana Chacon" (qty "Corroder" 5)]
                          :credits 2}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dadiana Chacon")
      (play-from-hand state :runner "Corroder")
      (is (last-n-log-contains? state 3 "Runner spends [Click] and pays 0 [Credits] to install Dadiana Chacon."))
      (is (last-n-log-contains? state 2 "Runner uses Dadiana Chacon to suffer 3 meat damage."))
      (is (second-last-log-contains? state "Runner trashes Corroder, Corroder, and Corroder due to meat damage."))
      (is (last-log-contains? state "Runner spends [Click] and pays 2 [Credits] to install Corroder."))))

(deftest daeg-first-net-cat
  ;; Daeg, First Net-Cat - charge on score/steal
  (do-game
    (new-game {:runner {:hand ["Daeg, First Net-Cat" "Earthrise Hotel"]}
               :corp {:hand ["Hostile Takeover"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (play-from-hand state :runner "Daeg, First Net-Cat")
    (take-credits state :runner)
    (let [hotel (get-resource state 0)]
      (play-and-score state "Hostile Takeover")
      (is (changed? [(get-counters (refresh hotel) :power) 1]
            (click-card state :runner "Earthrise Hotel"))
          "charge added 1 counter")))
  (do-game
    (new-game {:runner {:hand ["Daeg, First Net-Cat" "Earthrise Hotel"]}
               :corp {:hand ["Hostile Takeover"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Earthrise Hotel")
    (play-from-hand state :runner "Daeg, First Net-Cat")
    (let [hotel (get-resource state 0)]
      (run-empty-server state :hq)
      (click-prompt state :runner "Steal")
      (is (changed? [(get-counters (refresh hotel) :power) 1]
            (click-card state :runner "Earthrise Hotel"))
          "charge added 1 counter"))))


(deftest daily-casts
  ;; Daily Casts
  ;; Play and tick through all turns of daily casts
  (do-game
    (new-game {:runner {:deck [(qty "Daily Casts" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (let [dc (get-resource state 0)]
      ;; Number of credits
      (is (= 8 (get-counters dc :credit)))
      (is (= 2 (get-in @state [:runner :credit])))
      ;; End turn
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 6 (get-counters (refresh dc) :credit)))
      (is (= 7 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (get-counters (refresh dc) :credit)))
      (is (= 13 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh dc) :credit)))
      (is (= 19 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (nil? (get-resource state 0))))))

(deftest data-folding
  ;; Data Folding - Gain 1c at start of turn if 2+ unused MU
  (do-game
    (new-game {:runner {:deck ["Data Folding" "Hyperdriver"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Data Folding")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (core/available-mu state)) "At least 2 unused MU")
    (is (= 6 (:credit (get-runner))) "Gained 1c at turn start")
    (play-from-hand state :runner "Hyperdriver")
    (take-credits state :runner)
    (is (= 1 (core/available-mu state)) "Only 1 unused MU")
    (is (= 8 (:credit (get-runner))))
    (take-credits state :corp)
    (is (= 8 (:credit (get-runner))) "No credits gained at turn start")))

(deftest ddos
  ;; Prevent rezzing of outermost ice for the rest of the turn
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
               :runner {:deck ["DDoS"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "DDoS")
    (let [ddos (get-resource state 0)
          iwall (get-ice state :hq 1)]
      (card-ability state :runner ddos 0)
      (is (= (:title ddos) (get-in @state [:runner :discard 0 :title])))
      (run-on state "HQ")
      (rez state :corp iwall {:expect-rez false})
      (run-continue state)
      (is (not (rezzed? (refresh iwall))))
      (run-jack-out state)
      (run-on state "HQ")
      (rez state :corp iwall {:expect-rez false})
      (run-continue state)
      (is (not (rezzed? (refresh iwall))))
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state "HQ")
      (rez state :corp iwall)
      (run-continue state)
      (is (rezzed? (refresh iwall))))))

(deftest dean-lister
  ;; Basic test
  (do-game
    (new-game {:runner {:deck ["Dean Lister" "Faust" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Dean Lister")
    (play-from-hand state :runner "Faust")
    (run-on state :archives)
    (let [faust (get-program state 0)
          dean (get-resource state 0)]
      (is (= 2 (get-strength faust)) "Faust at 2 strength")
      (is (zero? (-> (get-runner) :discard count)) "Dean Lister not discarded yet")
      (card-ability state :runner dean 0)
      (click-card state :runner faust)
      (is (= 1 (-> (get-runner) :discard count)) "Dean Lister trashed to use its abilitiy")
      (is (= 5 (get-strength (refresh faust))) "Faust at 5 strength (2 base + 3 from Dean)")
      (card-ability state :runner faust 1) ;boost by 2
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 6 (get-strength (refresh faust))) "Faust at 6 strength (2 base + 2 from Dean + 2 from boost)")
      (run-jack-out state)
      (is (= 2 (get-strength (refresh faust))) "Dean Lister effect ends after run"))))

(deftest debbie-downtown-moreira
  (do-game
    (new-game {:corp {:hand ["Rashida Jaheem"]}
               :runner {:hand ["Carpe Diem" "Debbie \"Downtown\" Moreira"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Debbie \"Downtown\" Moreira")
    (let [deb (get-resource state 0)]
      (play-from-hand state :runner "Carpe Diem")
      (click-prompt state :runner "No")
      (is (= 1 (get-counters (refresh deb) :credit)) "1 credit placed on Debbie")
      (card-ability state :runner (refresh deb) 1)
      (click-prompt state :runner "HQ")
      (run-continue state)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (not (no-prompt? state :runner)) "Prompt to spend credits")
      (click-card state :runner (refresh deb))
      (is (no-prompt? state :runner) "Spent"))))

(deftest debbie-downtown-moreira-threat
  (do-game
    (new-game {:corp {:hand [(qty "Project Atlas" 2)]
                      :credits 10}
               :runner {:hand ["Debbie \"Downtown\" Moreira"]}})
    (play-and-score state "Project Atlas")
    (play-and-score state "Project Atlas")
    (take-credits state :corp)
    (play-from-hand state :runner "Debbie \"Downtown\" Moreira")
    (is (= 2 (get-counters (refresh (get-resource state 0)) :credit)) "Has 2 hosted credits")))

(deftest decoy
  ;; Decoy - Trash to avoid 1 tag
  (do-game
    (new-game {:corp {:deck ["SEA Source"]}
               :runner {:deck ["Decoy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Decoy")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (not (no-prompt? state :runner)) "Runner prompted to avoid tag")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (count (:discard (get-runner)))) "Decoy trashed")
    (is (zero? (count-tags state)) "Tag avoided")))

(deftest district-99-trashes-by-both-sides-and-manual-triggers
    ;; Trashes by both sides and manual triggers
    (do-game
      (new-game {:corp {:deck ["Bio-Ethics Association"]}
                 :runner {:deck ["District 99" (qty "Spy Camera" 3) "Faerie"]}})
      (play-from-hand state :corp "Bio-Ethics Association" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "District 99")
      (let [d99 (get-resource state 0)
            bea (get-content state :remote1 0)]
        (card-ability state :runner (refresh d99) 1) ; manually add power counter
        (is (= 1 (get-counters (refresh d99) :power)) "1 power counter was added manually")
        (card-ability state :runner (refresh d99) 1) ; try to manually add power counter twice
        (is (= 1 (get-counters (refresh d99) :power)) "Manual power counter addition is only possible once per turn")
        (play-from-hand state :runner "Spy Camera")
        (card-ability state :runner (get-hardware state 0) 1) ; pop spy camera
        (click-prompt state :runner "OK")
        (is (= 1 (get-counters (refresh d99) :power)) "Manual power counter addition suppressed later trigger")
        (play-from-hand state :runner "Spy Camera")
        (take-credits state :runner)
        (take-credits state :corp)
        (core/move-card state :runner {:card (find-card "Spy Camera" (:hand (get-runner)))
                                       :server "Heap"})
        (is (= 1 (get-counters (refresh d99) :power)) "Discarding from hand does not trigger D99")
        (is (= 1 (count (:hand (get-runner)))) "Faerie in hand")
        (is (= "Faerie" (:title (first (:hand (get-runner))))))
        (rez state :corp bea)
        (take-credits state :runner)
        (is (zero? (count (:hand (get-runner)))) "Faerie was trashed")
        (is (= 2 (get-counters (refresh d99) :power)) "Trashing Faerie from grip placed a counter")
        (card-ability state :runner (get-hardware state 0) 1) ; pop spy camera
        (click-prompt state :runner "OK")
        (is (= 2 (get-counters (refresh d99) :power)) "Trashing Spy Camera after Faerie did not place a counter"))))

(deftest district-99-rebirth-interaction-basic-functionality
    ;; Rebirth interaction, basic functionality
    (do-game
      (new-game {:corp {:deck ["Grim"]}
                 :runner {:id "Armand \"Geist\" Walker: Tech Lord"
                          :deck ["District 99" (qty "Spy Camera" 3) "Faerie" "Rebirth" "Sure Gamble"]}})
      (play-from-hand state :corp "Grim" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (click-draw state :runner)
      (click-draw state :runner)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "District 99")
      (play-from-hand state :runner "Rebirth")
      (let [khan "Khan: Savvy Skiptracer"]
        (click-prompt state :runner khan)
        (is (= khan (-> (get-runner) :identity :title)) "Rebirthed into Khan"))
      (play-from-hand state :runner "Spy Camera")
      (play-from-hand state :runner "Faerie")
      (let [d99 (get-resource state 0)
            faerie (get-program state 0)
            spycam (get-hardware state 0)
            grim (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp grim)
        (run-continue state)
        (card-subroutine state :corp (refresh grim) 0)
        (is (zero? (get-counters (refresh d99) :power)) "No power counters before Faerie is trashed")
        (click-card state :corp faerie)
        (is (= 1 (get-counters (refresh d99) :power)) "1 power counter was added for Faerie being trashed")
        (card-ability state :runner spycam 1) ; pop spycam
        (click-prompt state :runner "OK")
        (is (= 1 (get-counters (refresh d99) :power)) "Trashing Spy Camera after Faerie did not add a second power counter")
        (card-ability state :runner (refresh d99) 1) ; manually add counter
        (is (= 1 (get-counters (refresh d99) :power)) "Can't manually add power counter after one has already been added")
        (run-continue state :movement)
        (run-jack-out state)
        (play-from-hand state :runner "Spy Camera")
        (take-credits state :runner)
        (card-ability state :runner (get-hardware state 0) 1) ; pop spycam
        (click-prompt state :runner "OK")
        (is (= 2 (get-counters (refresh d99) :power)) "Trashing Spy Camera on Corp turn added a second power counter")
        (take-credits state :corp)
        (play-from-hand state :runner "Spy Camera")
        (take-credits state :runner)
        (card-ability state :runner (get-hardware state 0) 1) ; pop spycam
        (click-prompt state :runner "OK")
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh d99) :power)) "Trashing Spy Camera on Runner turn added a third power counter")
        (let [faerie (first (filter #(= (:title %) "Faerie") (:discard (get-runner))))]
          (doseq [c ["Sure Gamble" "Faerie" "Spy Camera"]]
            (is (some? (filter #(= (:title %) c) (:hand (get-runner)))) (str c " is in the discard")))
          (is (zero? (count (:hand (get-runner)))) "Faerie is not in hand")
          (card-ability state :runner (refresh d99) 0)  ; Retrieve card from Archives
          (is (= 2 (count (:choices (prompt-map :runner)))) "Runner can choose between Spy Camera and Faerie only")
          (click-prompt state :runner faerie)
          (is (= 1 (count (:hand (get-runner)))) "1 card added to hand")
          (is (= "Faerie" (-> (get-runner) :hand first :title)) "Faerie added to hand")
          (is (zero? (get-counters (refresh d99) :power)) "Picking up Faerie removed 3 counters")))))

(deftest district-99-harbinger-interaction-basic-functionality
    ;; Harbinger interaction, basic functionality
    (do-game
      (new-game {:corp {:deck ["Grim"]}
                 :runner {:deck ["District 99" "Harbinger" "Aesop's Pawnshop"]}})
      (play-from-hand state :corp "Grim" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "District 99")
      (play-from-hand state :runner "Aesop's Pawnshop")
      (play-from-hand state :runner "Harbinger")
      (let [d99 (get-resource state 0)
            ap (get-resource state 1)
            harb (get-program state 0)
            grim (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp grim)
        (run-continue state)
        (card-subroutine state :corp (refresh grim) 0)
        (is (zero? (get-counters (refresh d99) :power)) "No power counters before Harbinger is trashed")
        (click-card state :corp harb)
        (is (= 1 (get-counters (refresh d99) :power)) "1 power counter after Harbinger trashed")
        (run-continue state :movement)
        (run-jack-out state)
        (take-credits state :corp)
        (card-ability state :runner ap 0)
        (click-card state :runner (get-runner-facedown state 0))
        (is (= 1 (get-counters (refresh d99) :power)) "still 1 power counter after facedown Harbinger trashed"))))

(deftest district-99-interaction-with-maxx-5293
    ;; interaction with MaxX #5293
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :hand ["Sure Gamble" "Corroder" "District 99"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "District 99")
      (take-credits state :runner)
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Corroder" (:hand (get-runner))) :deck)
      (is (= "Corroder" (:title (nth (:deck (get-runner)) 1))))
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (last-log-contains? state "uses District 99 to place 1 power counter on itself") "D99 checks both cards")))

(deftest district-99-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp {:hand ["Blacklist"]}
                 :runner {:id "MaxX: Maximum Punk Rock"
                          :hand ["Sure Gamble" "District 99"]
                          :discard ["Sure Gamble" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "District 99")
      (is (not (find-card "Corroder" (:hand (get-runner)))) "Corroder is not in hand")
      (is (find-card "Corroder" (:discard (get-runner))) "Corroder is in heap")
      (core/add-counter state :runner (get-resource state 0) :power 3)
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 1 (count (prompt-buttons :runner))) "Only 1 card shown")
      (click-prompt state :runner "Corroder")
      (is (find-card "Corroder" (:hand (get-runner))))
      (is (no-prompt? state :runner) "Shuffle prompt did not come up")))

(deftest district-99-no-faction-cards-in-heap
    ;; No faction cards in heap
    (do-game
      (new-game {:corp {:hand ["Blacklist"]}
                 :runner {:id "MaxX: Maximum Punk Rock"
                          :hand ["Sure Gamble" "District 99"]
                          :discard ["Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "District 99")
      (core/add-counter state :runner (get-resource state 0) :power 3)
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "Add to hand prompt did not come up")))

(deftest district-99-heap-locked-test
    ;; Heap Locked Test
    (do-game
      (new-game {:corp {:hand ["Blacklist"]}
                 :runner {:id "MaxX: Maximum Punk Rock"
                          :hand ["Sure Gamble" "District 99"]
                          :discard ["Sure Gamble" "Corroder"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "District 99")
      (core/add-counter state :runner (get-resource state 0) :power 3)
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "Add to hand prompt did not come up")))

(let [;; Start id for dj-fenris
      sunny "Sunny Lebeau: Security Specialist"
      ;; Several G-mod identities
      geist "Armand \"Geist\" Walker: Tech Lord"
      hayley "Hayley Kaplan: Universal Scholar"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"
      maxx "MaxX: Maximum Punk Rock"]
  (deftest dj-fenris
    ;; DJ Fenris - host 1 g-mod id not in faction on DJ Fenris
    (testing "Hosting Chaos Theory"
      ;; Ensure +1 MU is handled correctly
      (do-game
        (new-game {:runner {:id sunny
                            :deck ["DJ Fenris"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (click-prompt state :runner chaos)
        (is (= chaos (get-in (get-resource state 0) [:hosted 0 :title])) "Chaos Theory hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Sunny, id not changed")
        (is (= 2 (get-link state)) "2 link from Sunny")
        (is (= 5 (core/available-mu state)) "+1 MU from Chaos Theory")
        ;; Trash DJ Fenris
        (trash-card state :runner (get-resource state 0))
        (is (not= chaos (-> (get-runner) :rfg last :title)) "Chaos Theory not moved to rfg")
        (is (not= chaos (-> (get-runner) :discard last :title)) "Chaos Theory not moved to discard")
        (is (not= chaos (-> (get-runner) :hand last :title)) "Chaos Theory not moved to hand")
        (is (not= chaos (-> (get-runner) :identity :title)) "Chaos Theory not moved to identity")
        (is (= 1 (count (:discard (get-runner)))) "1 card in heap: DJ Fenris")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")
        ;; Recover DJ Fenris
        (core/move state :runner (get-in (get-runner) [:discard 0]) :hand)
        (core/gain state :runner :credit 3)
        ;; Re-play DJ Fenris
        (play-from-hand state :runner "DJ Fenris")
        (is (some #(= chaos %) (prompt-titles :runner)) "Chaos Theory still available")
        ;; Try moving CT to hand
        (game.core/move state :runner (-> (get-resource state 0) :hosted first) :hand)
        (is (not= chaos (-> (get-runner) :rfg last :title)) "Chaos Theory not moved to rfg")
        (is (not= chaos (-> (get-runner) :discard last :title)) "Chaos Theory not moved to discard")
        (is (not= chaos (-> (get-runner) :hand last :title)) "Chaos Theory not moved to hand")
        (is (not= chaos (-> (get-runner) :identity :title)) "Chaos Theory not moved to identity")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")))
    (testing "Hosting Geist"
      ;; Ensure Geist effect triggers
      (do-game
        (new-game {:runner {:id sunny
                            :deck ["DJ Fenris" (qty "All-nighter" 3) (qty "Sure Gamble" 3)]}
                   :options {:start-as :runner}})
        (starting-hand state :runner ["DJ Fenris" "All-nighter" "All-nighter"])
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (click-prompt state :runner geist)
        (is (= geist (get-in (get-resource state 2) [:hosted 0 :title])) "Geist hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Sunny, id not changed")
        (is (= 2 (get-link state)) "2 link from Sunny, no extra link from Geist")
        (let [hand-count (count (:hand (get-runner)))]
          (card-ability state :runner (get-resource state 0) 0) ; Use All-nighter
          (is (= (inc hand-count) (count (:hand (get-runner))))
              "Drew one card with Geist when using All-nighter trash ability")
          (trash-card state :runner (find-card "DJ Fenris" (get-in @state [:runner :rig :resource])))
          (is (not= geist (-> (get-runner) :rfg last :title)) "Geist not moved to rfg")
          (is (not= geist (-> (get-runner) :discard last :title)) "Geist not moved to discard")
          (is (not= geist (-> (get-runner) :hand last :title)) "Geist not moved to hand")
          (is (not= geist (-> (get-runner) :identity :title)) "Geist not moved to identity")
          (is (= 2 (count (:discard (get-runner)))) "2 cards in heap: All-nighter and DJ Fenris")
          (card-ability state :runner (get-resource state 0) 0) ; Use All-nighter (again)
          (is (= (inc hand-count) (count (:hand (get-runner))))
              "Did not draw another card - Geist ability removed when DJ Fenris was trashed"))))
    (testing "Geist does not trigger Laguna Velasco"
      ;; Regression test for #3759
      (do-game
        (new-game {:runner {:id sunny
                            :deck ["DJ Fenris" "Laguna Velasco District" (qty "All-nighter" 3) (qty "Sure Gamble" 3)]}
                   :options {:start-as :runner}})
        (starting-hand state :runner ["DJ Fenris" "Laguna Velasco District" "All-nighter"])
        (core/gain state :runner :credit 10)
        (play-from-hand state :runner "All-nighter")
        (play-from-hand state :runner "Laguna Velasco District")
        (play-from-hand state :runner "DJ Fenris")
        (is (= (first (prompt-titles :runner)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [geist reina maxx hayley chaos]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie kate kit]))
        (click-prompt state :runner geist)
        (is (= geist (get-in (get-resource state 2) [:hosted 0 :title])) "Geist hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-runner)))) "Still Hayley, id not changed")
        (let [hand-count (count (:hand (get-runner)))]
          ;; Use All-nighter to trigger Geist
          (card-ability state :runner (get-resource state 0) 0)
          (is (= (inc hand-count) (count (:hand (get-runner))))
              "Drew one card with Geist when using All-nighter trash ability, not two (from Laguna Velasco District)"))))
    (testing "Disable with Malia"
      (do-game
        (new-game {:corp {:deck ["Malia Z0L0K4"]}
                   :runner {:id geist
                            :deck ["DJ Fenris"]}})
        (play-from-hand state :corp "Malia Z0L0K4" "New remote")
        (take-credits state :corp)
        (play-from-hand state :runner "DJ Fenris")
        (click-prompt state :runner chaos)
        (is (= 5 (core/available-mu state)) "Gained 1 MU from CT")
        (let [malia (get-content state :remote1 0)
              dj-fenris (get-resource state 0)
              hosted-ct #(first (:hosted (refresh dj-fenris)))]
          (rez state :corp malia)
          (click-card state :corp dj-fenris)
          (is (= 4 (core/available-mu state)) "Disabling DJ Fenris also disabled CT, reducing MU back to 4")
          ;; Trash Malia to stop disable
          (trash state :corp (refresh malia))
          (is (not (:disabled (refresh dj-fenris))) "DJ Fenris is enabled")
          (is (not (:disabled (hosted-ct))) "CT is enabled")
          (is (= 5 (core/available-mu state)) "Enabling DJ Fenris also enabled CT, bringing MU back up to 5"))))
    (testing "Interaction with Paige Piper #4055"
      (do-game
        (new-game {:runner {:id sunny
                            :deck [(qty "Sure Gamble" 5) (qty "DJ Fenris" 2)]
                            :hand ["DJ Fenris" "Paige Piper"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Paige Piper")
        (click-prompt state :runner "No")
        (take-credits state :runner)
        (take-credits state :corp)
        (play-from-hand state :runner "DJ Fenris")
        (click-prompt state :runner "Paige Piper")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner chaos)
        (is (no-prompt? state :corp))
        (is (no-prompt? state :runner))))
    (testing "IDs disabled if DJ Fenris is facedown #6058"
      (do-game
       (new-game {:runner {:id sunny
                           :deck [(qty "Sure Gamble" 5)]
                           :hand ["DJ Fenris" "Apocalypse"]
                           :credits 10}
                  :corp {:deck [(qty "Hedge Fund" 5)]}})
       (take-credits state :corp)
       (play-from-hand state :runner "DJ Fenris")
       (click-prompt state :runner chaos)
       (is (= 5 (core/available-mu state)) "Gained 1 MU from CT")
       (core/gain-clicks state :runner 1)
       (run-empty-server state "Archives")
       (run-empty-server state "R&D")
       (run-empty-server state "HQ")
       (click-prompt state :runner "No action")
       (play-from-hand state :runner "Apocalypse")
       (let [dj-fenris (get-runner-facedown state 0)
             hosted-ct #(first (:hosted (refresh dj-fenris)))]
         (is (core/facedown? dj-fenris) "DJ Fenris is facedown")
         (is (core/facedown? (hosted-ct)) "CT is facedown")
         (is (= 4 (core/available-mu state)) "CT not active since DJ Fenris is facedown, reducing MU back to 4"))))
    (testing "Only legal IDs appear in the drop down"
      (do-game
       (new-game {:runner {:id sunny
                           :deck [(qty "Sure Gamble" 5)]
                           :hand ["DJ Fenris"]
                           :credits 10}
                  :corp {:deck [(qty "Hedge Fund" 5)]}
                  :options {:format :standard}})
       (take-credits state :corp)
       (play-from-hand state :runner "DJ Fenris")
       (is (every? #(legal? :standard :legal %) (prompt-buttons :runner)) "Only legal IDs are available")))))

(deftest donut-taganes
  ;; Donut Taganes - add 1 to play cost of Operations & Events when this is in play
  (do-game
    (new-game {:runner {:deck ["Donut Taganes" "Easy Mark"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Donut Taganes")
    (is (= 2 (:credit (get-runner))) "Donut played for 3c")
    (play-from-hand state :runner "Easy Mark")
    (is (= 4 (:credit (get-runner))) "Easy Mark only gained 2c")
    (take-credits state :runner)
    (is (= 8 (:credit (get-corp))) "Corp has 8c")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 11 (:credit (get-corp))) "Corp has 11c")))

(deftest dr-lovegood
  (do-game
      (new-game {:runner {:deck ["Dr. Lovegood" "Virus Breeding Ground" "The Black File" "Drug Dealer"]
                          :credits 40}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Dr. Lovegood")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [blackfile (get-resource state 0)
            vbg (get-resource state 1)]
        (click-prompt state :runner "Dr. Lovegood")
        (click-card state :runner blackfile)
        (is (:icon (refresh blackfile)) "The Black File has an icon")
        (is (= 0 (get-counters (refresh blackfile) :power)) "Black File has still 0 power counters")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground has 1 virus counter")
        (take-credits state :runner)
        (is (nil? (:icon (refresh blackfile)))))))

(deftest dr-nuka-vrolyck
  (do-game
      (new-game {:runner {:hand ["Dr. Nuka Vrolyck"]
                          :deck [(qty "Sure Gamble" 5)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dr. Nuka Vrolyck")
      (let [dr (get-resource state 0)]
        (is (changed? [(count (:hand (get-runner))) 3]
              (card-ability state :runner dr 0))
            "Runner has drawn 3 cards")
        (is (= 2 (:click (get-runner))))
        (is (changed? [(count (:hand (get-runner))) 2]
              (card-ability state :runner dr 0))
            "Runner has drawn the remaining 2 cards from the stack")
        (is (= 1 (count (:discard (get-runner)))) "Dr. Nuka Vrolyck was trashed"))))

(deftest dreamnet-draw-1-card-on-first-successful-run
    ;; Draw 1 card on first successful run
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["DreamNet"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DreamNet")
      (is (empty? (:hand (get-runner))) "Runner has 0 cards in hand")
      (is (changed? [(count (:hand (get-runner))) 1]
            (run-empty-server state :archives))
          "Runner has drawn 1 card on successful run")))

(deftest dreamnet-don-t-draw-anything-on-runs-after-the-first
    ;; Don't draw anything on runs after the first
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["DreamNet"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DreamNet")
      (is (empty? (:hand (get-runner))) "Runner has 0 cards in hand")
      (run-empty-server state :archives)
      (is (= 1 (count (:hand (get-runner)))) "Runner has drawn 1 card on successful run")
      (is (changed? [(count (:hand (get-runner))) 0]
            (run-empty-server state :archives))
          "Runner has not drawn additional cards")))

(deftest dreamnet-don-t-draw-anything-on-unsuccessful-run
    ;; Don't draw anything on unsuccessful run
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["DreamNet"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "DreamNet")
      (is (changed? [(count (:hand (get-runner))) 0]
            (run-on state :archives)
            (run-jack-out state))
          "Runner still has 0 cards in hand")))

(deftest dreamnet-gain-1-credit-on-successful-run
    ;; Gain 1 credit on successful run
    (testing "with 2 link"
      (do-game
        (new-game {:runner {:id "Sunny Lebeau: Security Specialist"
                            :deck [(qty "Sure Gamble" 5)]
                            :hand ["DreamNet"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "DreamNet")
        (is (changed? [(:credit (get-runner)) 1]
              (run-empty-server state :archives))
            "Runner gains 1 credit for having 2 link")))
    (testing "with Digital subtype"
      (do-game
        (new-game {:runner {:id "Apex: Invasive Predator"
                            :deck [(qty "Sure Gamble" 5)]
                            :hand ["DreamNet"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "DreamNet")
        (is (changed? [(:credit (get-runner)) 1]
              (run-empty-server state :archives))
            "Runner gains 1 credit for having Digital subtype"))))

(deftest dummy-box
  ;; Dummy Box - trash a card from hand to prevent corp trashing installed card
  (do-game
      (new-game {:runner {:deck [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dummy Box")
      (play-from-hand state :runner "Cache")
      (take-credits state :runner)
      (trash state :runner (get-program state 0))
      (is (not (no-prompt? state :runner)) "Dummy Box prompting to prevent program trash")
      (card-ability state :runner (get-resource state 0) 1)
      (click-card state :runner (find-card "Clot" (:hand (get-runner))))
      (click-prompt state :runner "Done")
      (is (= 1 (count (:discard (get-runner)))) "Clot trashed")
      (is (empty? (:hand (get-runner))) "Card trashed from hand")
      (is (= 1 (count (get-program state))) "Cache still installed")
      (is (= 1 (count (get-resource state))) "Dummy Box still installed")))

(deftest dummy-box-doesn-t-prevent-program-deletion-during-purge
    ;; doesn't prevent program deletion during purge
    (do-game
      (new-game {:runner {:deck [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dummy Box")
      (play-from-hand state :runner "Clot")
      (take-credits state :runner)
      (purge state :corp)
      (is (no-prompt? state :runner) "Dummy Box not prompting to prevent purge trash")))

(deftest dummy-box-don-t-prevent-trashing-from-hand
    ;; don't prevent trashing from hand
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Merger" "Ibrahim Salem"]
                        :credits 10}
                 :runner {:hand ["Dummy Box" "Cache"]}})
      (core/gain state :corp :click 5)
      (play-and-score state "Merger")
      (play-from-hand state :corp "Ibrahim Salem" "New remote")
      (rez state :corp (get-content state :remote2 0) {:expect-rez false})
      (click-card state :corp "Merger")
      (take-credits state :corp)
      (play-from-hand state :runner "Dummy Box")
      (take-credits state :runner)
      (card-ability state :corp (get-content state :remote2 0) 0)
      (click-prompt state :corp "Program")
      (click-prompt state :corp "Cache")
      (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trashing from hand")))

(deftest earthrise-hotel
  ;; Earthrise Hotel
  (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Earthrise Hotel"]}})
      (play-from-hand state :runner "Earthrise Hotel")
      (let [eh (get-resource state 0)]
        (is (zero? (count (:hand (get-runner)))) "Runner has no cards in hand")
        (is (= 3 (get-counters (refresh eh) :power)) "Earthrise starts with 3 power counters")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (count (:hand (get-runner)))) "Runner draws 2 cards")
        (is (= 2 (get-counters (refresh eh) :power)) "Earthrise loses 1 power counter")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 4 (count (:hand (get-runner)))) "Runner draws 2 cards")
        (is (= 1 (get-counters (refresh eh) :power)) "Earthrise loses 1 power counter")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 6 (count (:hand (get-runner)))) "Runner draws 2 cards")
        (is (nil? (refresh eh)) "Earthrise is no longer installed")
        (is (= 1 (count (:discard (get-runner)))) "Earthrise is in heap"))))

(deftest earthrise-hotel-hanging-prompts-issue-4113
    ;; Hanging prompts. Issue #4113
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Earthrise Hotel"]}})
      (play-from-hand state :runner "Earthrise Hotel")
      (is (zero? (count (:hand (get-runner)))) "Runner has no cards in hand")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (no-prompt? state :runner) "Runner has no prompts open")
      (is (no-prompt? state :corp) "Corp has no prompts open")
      (is (= 2 (count (:hand (get-runner)))) "Runner draws 2 cards")))

(deftest eden-shard
  ;; Eden Shard - Install from Grip in lieu of accessing R&D; trash to make Corp draw 2
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Eden Shard"]}})
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (run-empty-server state :rd)
      (click-prompt state :runner "Eden Shard")
      (is (= 5 (:credit (get-runner))) "Eden Shard installed for 0c")
      (is (not (:run @state)) "Run is over")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 3 (count (:hand (get-corp)))) "Corp drew 2 cards")
      (is (= 1 (count (:discard (get-runner)))) "Eden Shard trashed")))

(deftest eden-shard-do-not-install-when-accessing-cards
    ;; Do not install when accessing cards
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck ["Eden Shard"]}})
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (run-empty-server state :rd)
      (click-prompt state :runner "Breach R&D")
      (click-prompt state :runner "No action")
      (is (not (get-resource state 0)) "Eden Shard not installed")
      (is (= 1 (count (:hand (get-runner)))) "Eden Shard not installed")))

(deftest enhanced-vision-logs-the-revealed-card
    ;; Logs the revealed card
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hostile Takeover"]}
                 :runner {:hand ["Enhanced Vision"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Enhanced Vision")
      (run-empty-server state "Archives")
      (is (second-last-log-contains? state "uses Enhanced Vision to force the Corp to reveal Hostile Takeover")
          "Card name is logged")))

(deftest enhanced-vision-triggers-reveal-abilities
    ;; Triggers reveal abilities
    (do-game
      (new-game {:corp {:id "Hyoubu Institute: Absolute Clarity"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Hostile Takeover"]}
                 :runner {:hand ["Enhanced Vision"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Enhanced Vision")
      (is (changed? [(:credit (get-corp)) 1]
            (run-empty-server state "Archives"))
          "Corp gains 1 from Enhanced Vision forced reveal")))

(deftest environmental-testing
  ;; Pops at 4 power counters
  (do-game
    (new-game {:runner {:hand ["Environmental Testing" (qty "Spy Camera" 2) (qty "Marjanah" 2)]}})
    (take-credits state :corp)
    (core/gain-clicks state :runner 5)
    (play-from-hand state :runner "Environmental Testing")
    (let [env (get-resource state 0)]
      (is (= 0 (get-counters (refresh env) :power)) "starts at 0 counters")
      (is (changed? [(get-counters (refresh env) :power) 1]
            (play-from-hand state :runner "Spy Camera"))
          "+1 from hardware")
      (is (changed? [(get-counters (refresh env) :power) 1]
            (play-from-hand state :runner "Spy Camera"))
          "+1 from hardware")
      (is (changed? [(get-counters (refresh env) :power) 1]
            (play-from-hand state :runner "Marjanah"))
          "+1 from program")
      (is (changed? [(:credit (get-runner)) 9]
            (play-from-hand state :runner "Marjanah")
            (is (find-card "Environmental Testing" (:discard (get-runner)))
            "Env. Testing was trashed"))
          "Env Testing pops for 9c"))))

(deftest eru-ayase-pessoa
  (do-game
    (new-game {:corp {:hand ["IPO" "IPO" "City Works Project"]
                      :deck ["Hedge Fund" "Hedge Fund" "Hedge Fund"]}
               :runner {:hand ["Eru Ayase-Pessoa" "Divide and Conquer"]}})
    (play-and-score state "City Works Project")
    (take-credits state :corp)
    (play-from-hand state :runner "Eru Ayase-Pessoa")
    (let [eru (get-resource state 0)]
      (is (changed? [(count-tags state) 1]
            (card-ability state :runner (refresh eru) 0))
          "Got 1 tag")
      (run-continue state)
      ;;should access two cards
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (empty? (get-run)) "Run has ended")
      ;;1 from hq 2 from rd
      (play-from-hand state :runner "Divide and Conquer")
      (run-continue state :success)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (empty? (get-run)) "Run has ended"))))

(deftest fan-site
  ;; Fan Site - Add to score area as 0 points when Corp scores an agenda
  (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Fan Site"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-runner))))
      (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")))

(deftest fan-site-don-t-trigger-after-swap-with-exchange-of-information-issue-1824
    ;; Don't trigger after swap with Exchange of Information. Issue #1824
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Merger" "Exchange of Information"]}
                 :runner {:deck ["Fan Site"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Exchange of Information")
      (click-card state :corp "Fan Site")
      (click-card state :corp "Hostile Takeover")
      (is (= 1 (:agenda-point (get-runner))))
      (is (zero? (:agenda-point (get-corp))))
      (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site swapped into Corp score area")
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (is (find-card "Fan Site" (:scored (get-corp))) "Fan Site not removed from Corp score area")))

(deftest fan-site-runner-can-forfeit-fan-site
    ;; Runner can forfeit Fan Site
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Fan Site" "Data Dealer"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Fan Site")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-runner))))
      (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")
      (take-credits state :corp)
      (play-from-hand state :runner "Data Dealer")
      (let [credits (:credit (get-runner))]
        (card-ability state :runner (get-resource state 0) 0)
        (click-card state :runner (get-scored state :runner 0))
        (is (zero? (count (:scored (get-runner)))) "Fan Site successfully forfeit to Data Dealer")
        (is (= (+ credits 9) (:credit (get-runner))) "Gained 9 credits from Data Dealer"))))

(deftest fencer-fueno
  ;; Fencer Fueno - Companion, credits usable only during successful runs (after accessing server)
  (do-game
      (new-game {:corp {:hand ["Hostile Takeover" "PAD Campaign"]}
                 :runner {:hand ["Fencer Fueno"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Fencer Fueno")
      (let [ff (get-resource state 0)]
        (core/add-counter state :runner (refresh ff) :credit 4)
        (is (= 4 (get-counters (refresh ff) :credit)) "Fencer counters added")
        (let [credits (:credit (get-runner))
              counters (get-counters (refresh ff) :credit)]
          (run-on state "Server 1")
          (card-ability state :runner ff 0)
          (is (= credits (:credit (get-runner))) "Can't use credits on Fencer before a successul run")
          (run-continue state)
          (card-ability state :runner ff 0)
          (is (= (dec counters) (get-counters (refresh ff) :credit)) "Spent 1c from Fencer")
          (is (= (inc credits) (:credit (get-runner))) "Used credits from Fencer for trash")
          (click-prompt state :runner "Pay 4 [Credits] to trash")
          (click-prompt state :runner "Done")) ; pay-credits prompt
        (take-credits state :runner)
        (let [credits (:credit (get-runner))]
          (click-prompt state :runner "Pay 1 [Credits]")
          (is (= (dec credits) (:credit (get-runner))) "Paid 1c to not trash Fencer")
          (is (refresh ff) "Fencer not trashed")
          (is (not (find-card "Fencer Fueno" (:discard (get-runner)))) "Fencer not in discard yet")
          (take-credits state :corp)
          (take-credits state :runner))
        (let [credits (:credit (get-runner))]
          (click-prompt state :runner "Trash Fencer Fueno")
          (is (= credits (:credit (get-runner))) "Didn't pay to trash Fencer")
          (is (nil? (refresh ff)) "Fencer not installed")
          (is (find-card "Fencer Fueno" (:discard (get-runner))) "Fencer trashed")))))

(deftest fencer-fueno-pay-credits-prompt
    ;; pay-credits prompt
    (do-game
      (new-game {:corp {:hand ["Pop-up Window" "PAD Campaign"]}
                 :runner {:hand ["Fencer Fueno"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Pop-up Window" "Server 1")
      (take-credits state :corp)
      (play-from-hand state :runner "Fencer Fueno")
      (core/lose state :runner :credit 5) ;no money besides fueno
      (let [ff (get-resource state 0)
            popup (get-ice state :remote1 0)]
        (core/add-counter state :runner (refresh ff) :credit 4)
        (is (= 4 (get-counters (refresh ff) :credit)) "Fencer counters added")
        (run-on state "Server 1")
        (rez state :corp popup)
        (run-continue state)
        (core/play-runner-ability state :runner {:card popup
                                                 :ability 0
                                                 :targets nil})
        (is (no-prompt? state :runner) "No prompt for Fueno")
        (run-continue state)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) 0]
              (click-prompt state :runner "Pay 4 [Credits] to trash")
              (dotimes [_ 4] (click-card state :runner ff)))
            "Used 4 credit from Fencer Fueno"))))

(deftest fencer-fueno-pay-credits-gagarin
    ;; pay-credits + Gagarin
    (do-game
      (new-game {:corp {:id "Gagarin Deep Space: Expanding the Horizon"
                        :deck ["PAD Campaign"]}
                 :runner {:hand ["Fencer Fueno"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/lose state :runner :credit 5) ;no money besides fueno
      (play-from-hand state :runner "Fencer Fueno")
      (let [ff (get-resource state 0)]
        (core/add-counter state :runner (refresh ff) :credit 5)
        (is (= 5 (get-counters (refresh ff) :credit)) "Fencer counters added")
        (run-empty-server state "Server 1")
        (is (changed? [(:credit (get-runner)) 0]
              (click-prompt state :runner "Pay to access")
              (click-card state :runner ff)
              ; pay Gagarin credit
              (click-prompt state :runner "Pay 4 [Credits] to trash")
              (dotimes [_ 4] (click-card state :runner ff)))
            "Used 1 credit from Fencer Fueno"))))

(deftest fester
  ;; Fester - Corp loses 2c (if able) when purging viruses
  (do-game
    (new-game {:runner {:deck ["Fester"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Fester")
    (take-credits state :runner)
    (core/lose state :corp :credit 5)
    (core/gain state :corp :click 3)
    (is (= 3 (:credit (get-corp))))
    (purge state :corp)
    (is (= 1 (:credit (get-corp))) "Lost 2c when purging")
    (purge state :corp)
    (is (= 1 (:credit (get-corp))) "Lost no credits when purging, only had 1c")))

(deftest film-critic-prevent-corp-trashed-execs-going-to-runner-scored-issues-1181-1042
    ;; Prevent Corp-trashed execs going to Runner scored. Issues #1181/#1042
    (do-game
      (new-game {:corp {:deck [(qty "Director Haas" 3) (qty "Project Vitruvius" 3) "Hedge Fund"]}
                 :runner {:deck ["Film Critic"]}})
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (trash-from-hand state :corp "Director Haas")
        (is (= 1 (count (:discard (get-corp)))) "Director Haas stayed in Archives")
        (is (zero? (:agenda-point (get-runner))) "No points gained by Runner")
        (is (empty? (:scored (get-runner))) "Nothing in Runner scored"))))

(deftest film-critic-fetal-ai-interaction
    ;; Fetal AI interaction
    (do-game
      (new-game {:corp {:deck [(qty "Fetal AI" 3)]}
                 :runner {:deck ["Film Critic" (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state "HQ")
        ;; should not have taken damage yet
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt yet")
        (click-prompt state :runner "Yes")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (card-ability state :runner fc 0)
        (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner scored")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))

(deftest film-critic-do-not-take-a-net-damage-when-a-hosted-agenda-is-trashed-due-to-film-critic-trash-2382
    ;; Do not take a net damage when a hosted agenda is trashed due to film critic trash #2382
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 3) "Project Vitruvius"]}
                 :runner {:deck ["Film Critic" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (run-empty-server state :remote2)
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (core/gain state :corp :credit 10)
        (gain-tags state :runner 1)
        (trash-resource state)
        (click-card state :corp fc)
        (is (= 1 (count (:discard (get-runner)))) "FC trashed")
        (is (= 1 (count (:discard (get-corp)))) "Agenda trashed")
        (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))

(deftest film-critic-required-hosted-cards-to-be-an-agenda-before-firing-ability
    ;; required hosted cards to be an agenda before firing ability
    (do-game
      (new-game {:corp {:deck ["MCA Informant"]}
                 :runner {:deck ["Film Critic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (let [fc (first (get-resource state))]
        (take-credits state :runner)
        (play-from-hand state :corp "MCA Informant")
        (click-card state :corp fc)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant hosted on FC")
        (take-credits state :corp)
        (card-ability state :runner fc 0)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant still hosted on FC"))))

(deftest film-critic-remove-hosted-advancement-tokens
    ;; remove hosted advancement tokens
    (do-game
      (new-game {:corp {:deck ["Priority Requisition"]}
                 :runner {:deck ["Film Critic"]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (let [prireq (get-content state :remote1 0)]
        (dotimes [_ 2] (click-advance state :corp (refresh prireq)))
        (take-credits state :corp)
        (play-from-hand state :runner "Film Critic")
        (run-empty-server state :remote1)
        (is (= 2 (get-counters (refresh prireq) :advancement)) "Two advancement tokens on Pri Req")
        (click-prompt state :runner "Yes")
        (is (= 0 (get-counters (refresh prireq) :advancement)) "No more counters on the Pri Req"))))

(deftest film-critic-kill-switch-interaction-issue-4641
    ;; Kill Switch interaction. Issue #4641
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                        :hand ["Kill Switch" "Hostile Takeover"]}
                 :runner {:deck ["Film Critic" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Kill Switch")
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Yes")
      (is (no-prompt? state :corp))
      (is (no-prompt? state :corp))))

(deftest film-critic-shouldn-t-register-events-unless-marked-5019
    ;; Shouldn't register events unless marked. #5019
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Better Citizen Program"]}
                 :runner {:hand ["Film Critic" "Dirty Laundry"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Film Critic")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Yes")
      (card-ability state :runner (get-resource state 0) 0)
      (play-from-hand state :runner "Dirty Laundry")
      (is (zero? (count-tags state)) "Runner doesn't gain a tag from BCP")))

(deftest find-the-truth
  ;; Find the Truth
  (do-game
      (new-game {:corp {:deck [(qty "Restructure" 10)]
                        :hand ["Restructure"]}
                 :runner {:deck ["Find the Truth"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Find the Truth")
      (run-empty-server state "HQ")
      (is (= "Look at the top card of R&D?" (:msg (prompt-map :runner))) "FTT prompt")
      (click-prompt state :runner "Yes")
      (is (= "The top card of R&D is Restructure" (:msg (prompt-map :runner))) "FTT shows card on R&D")
      (click-prompt state :runner "OK")))

(deftest find-the-truth-equivocation-ftt-should-get-order-of-choice
    ;; Equivocation & FTT - should get order of choice
    (do-game
      (new-game {:corp {:deck [(qty "Restructure" 10)]
                        :hand ["Restructure"]}
                 :runner {:deck ["Equivocation" "Find the Truth"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Equivocation")
      (play-from-hand state :runner "Find the Truth")
      (run-empty-server state :rd)
      (click-prompt state :runner "Find the Truth")
      (is (= "Look at the top card of R&D?" (:msg (prompt-map :runner))) "FTT prompt")
      (click-prompt state :runner "Yes")
      (is (= "The top card of R&D is Restructure" (:msg (prompt-map :runner))) "FTT shows card")
      (click-prompt state :runner "OK") ; Equivocation prompt
      (is (= "Reveal the top card of R&D?" (:msg (prompt-map :runner))) "Equivocation Prompt")
      (click-prompt state :runner "Yes")))

(deftest find-the-truth-find-the-truth-should-completed-before-marilyn-trash-is-forced
    ;; Find The Truth should completed before Marilyn trash is forced
    (do-game
      (new-game {:corp {:deck ["Marilyn Campaign" (qty "Vanilla" 10)]}
                 :runner {:deck ["Find the Truth" "Neutralize All Threats"]}})
      (starting-hand state :corp ["Marilyn Campaign"])
      (play-from-hand state :corp "Marilyn Campaign" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (is (= 3 (:credit (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Find the Truth")
      (play-from-hand state :runner "Neutralize All Threats")
      (run-empty-server state :remote1)
      (is (= "Look at the top card of R&D?" (:msg (prompt-map :runner))) "FTT prompt")
      (is (= "Waiting for Runner to resolve pending triggers" (:msg (prompt-map :corp))) "No Marilyn Shuffle Prompt")
      (click-prompt state :runner "Yes")
      (is (= "The top card of R&D is Vanilla" (:msg (prompt-map :runner))) "FTT shows card")
      (is (= "Waiting for Runner to resolve pending triggers" (:msg (prompt-map :corp))) "No Marilyn Shuffle Prompt")
      (click-prompt state :runner "OK")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= :waiting (prompt-type :runner))
          "Runner has prompt to wait for Corp to shuffle Marilyn")
      (is (= "Shuffle Marilyn Campaign into R&D?" (:msg (prompt-map :corp))) "Now Corp gets shuffle choice")
      (is (= 2 (:credit (get-runner)))) #_ trashed_marilyn))

(deftest friend-of-a-friend
  (do-game
    (new-game {:runner {:hand [(qty "Friend of a Friend" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Friend of a Friend")
    (is (changed? [(:credit (get-runner)) 9
                   (count-tags state) 1]
                  (card-ability state :runner (get-resource state 0) 1))
        "Runner gains 9 credits a 1 tag")
    (play-from-hand state :runner "Friend of a Friend")
    (is (changed? [(:credit (get-runner)) 5
                   (count-tags state) -1]
                  (card-ability state :runner (get-resource state 0) 0))
        "Runner gains 5 credits and loses 1 tag")
    (is (= 2 (count (:discard (get-runner)))) "Both trashed")))

(deftest gang-sign-accessing-from-hq-not-including-root-issue-2113
    ;; accessing from HQ, not including root. Issue #2113
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 3) (qty "Braintrust" 2) "Crisium Grid"]}
                 :runner {:deck [(qty "Gang Sign" 2) "HQ Interface"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "HQ Interface")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Gang Sign") ; simultaneous effect resolution
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Steal")))

(deftest gang-sign-has-the-correct-prompts-issue-2113
    ;; Has the correct prompts. Issue #2113
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Snare!"]}
                 :runner {:deck ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      ;; Runner has "wait for Snare, wait for on-access" prompts.
      (is (= 2 (count (:prompt (get-runner)))) "Runner only has the Waiting prompt, not Snare!'s pay-prompt")
      ;; Core has "pay for Snare, wait for agenda-scored" prompts.
      (is (= 2 (count (:prompt (get-corp)))) "Corp has the prompt to use Snare!")))

(deftest gang-sign-active-player-gets-their-prompts-first-5033
    ;; Active player gets their prompts first. #5033
    (do-game
      (new-game {:corp {:hand ["Hostile Takeover" "Cyberdex Sandbox"]}
                 :runner {:hand ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (play-and-score state "Cyberdex Sandbox")
      (is (= "Purge virus counters?" (:msg (prompt-map :corp)))
          "Corp has the first ability prompt")
      (is (= ["Yes" "No"] (prompt-buttons :corp)) "Corp has Cyberdex Sandbox optional")
      (is (prompt-is-type? state :runner :waiting))
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Steal")))

(deftest gbahali
  ;; Gbahali
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma"]}
               :runner {:hand [(qty "Gbahali" 2)]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Gbahali")
    (play-from-hand state :runner "Gbahali")
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (let [gbahali-1 (get-resource state 0)
          gbahali-2 (get-resource state 1)
          enigma (get-ice state :hq 0)]
      (card-ability state :runner (refresh gbahali-1) 0)
      (is (nil? (refresh gbahali-1)) "Gbahali is trashed")
      (is (:broken (last (:subroutines (refresh enigma))))
          "Enigma's ETR should be broken")
      (is (not (:broken (first (:subroutines (refresh enigma)))))
          "Enigma's Lose a Click should not be broken")
      (card-ability state :runner (refresh gbahali-2) 0)
      (is (refresh gbahali-2)
          "Second Gbahali isn't trashed as last subroutine is already broken"))))

(deftest gene-conditioning-shoppe
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twice flag
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:deck ["Gene Conditioning Shoppe"
                                 "Adjusted Chronotype"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
      (trash state :runner (get-resource state 1))
      (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))))

(deftest gene-conditioning-shoppe-set-genetics-trigger-twice-flag-ensure-redundant-copies-work
    ;; set :genetics-trigger-twice flag - ensure redundant copies work
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Gene Conditioning Shoppe" 2)
                                 "Adjusted Chronotype"]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Adjusted Chronotype")
      (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice)))
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (let [gcs1 (get-resource state 1)
            gcs2 (get-resource state 2)]
          (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
          (trash state :runner gcs1)
          (is (core/has-flag? state :runner :persistent :genetics-trigger-twice))
          (trash state :runner gcs2)
          (is (not (core/has-flag? state :runner :persistent :genetics-trigger-twice))))))

(deftest ghost-runner-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Ghost Runner" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Ghost Runner")
      (play-from-hand state :runner "Refractor")
      (run-on state :hq)
      (let [gr (get-resource state 0)
            refr (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 2]
              (dotimes [_ 2]
                             (card-ability state :runner gr 0)))
            "Took 2 credits off of Ghost runner the traditional way.")
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner refr 1)
              (click-card state :runner gr))
            "Used 1 credit from Ghost Runner")
        (is (not-empty (:discard (get-runner))) "Empty Ghost Runner trashed"))))

(deftest ghost-runner-can-be-used-in-psi-games-issue-1149
    ;; Can be used in psi games. Issue #1149
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Snowflake"]}
                 :runner {:hand ["Ghost Runner"]
                          :credits 1}})
      (play-from-hand state :corp "Snowflake" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Ghost Runner")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (let [sf (get-ice state :hq 0)
            gr (get-resource state 0)]
        (card-subroutine state :corp sf 0)
        (is (zero? (:credit (get-runner))) "Runner has 0 credits")
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "2 [Credits]")
        (click-card state :runner gr)
        (click-card state :runner gr)
        (is (zero? (:credit (get-runner))) "Runner still has 0 credits")
        (is (= 1 (get-counters (refresh gr) :credit)) "Ghost Runner now has only 1 credit"))))

(deftest globalsec-security-clearance
  ;; Globalsec Security Clearance - Ability, click lost on use
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3)]
                      :hand ["Hedge Fund"]}
               :runner {:deck ["Globalsec Security Clearance"]}})
    (take-credits state :corp)
    (swap! state assoc-in [:runner :identity :baselink] 2)
    (core/fake-checkpoint state)
    (play-from-hand state :runner "Globalsec Security Clearance")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [gsec (get-resource state 0)]
      (card-ability state :runner gsec 0)
      (click-prompt state :runner "Yes")
      (is (= "The top card of R&D is Hedge Fund" (:msg (prompt-map :runner))) "GSec revealed Hedge Fund")
      (click-prompt state :runner "OK")
      (end-phase-12 state :runner)
      (is (= 3 (:click (get-runner))) "Runner lost 1 click from Globalsec Security Clearance"))))

(deftest grifter
  ;; Grifter - Gain 1c if you made a successful run this turn, otherwise trash it
  (do-game
    (new-game {:runner {:deck ["Grifter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Grifter")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (take-credits state :runner)
    (is (= 6 (:credit (get-runner))) "Gained 1c for a successful run during the turn")
    (take-credits state :corp)
    (run-on state :hq)
    (run-jack-out state)
    (take-credits state :runner)
    (is (= 1 (count (:discard (get-runner)))) "No successful runs; Grifter is trashed")))

(deftest grifter-trash-does-not-trigger-dummy-box
  ;; Grifter trash doesn't trigger Dummy Box
  (do-game
    (new-game {:runner {:deck [(qty "Grifter" 2) "Dummy Box"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Grifter")
    (play-from-hand state :runner "Dummy Box")
    (take-credits state :runner)
    (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trash")))

(deftest guru-davinder-no-prompt-trash-for-preventing-0-damage
    ;; no prompt/trash for preventing 0 damage
    (do-game
      (new-game {:corp {:deck ["Punitive Counterstrike"]}
                 :runner {:deck ["Guru Davinder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Guru Davinder")
      (take-credits state :runner)
      (play-from-hand state :corp "Punitive Counterstrike")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (no-prompt? state :runner)
          "There is no prompt for 0 damage")))

(deftest guru-davinder-cannot-steal-obokata-while-installed
    ;; cannot steal Obokata while installed
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Obokata Protocol" 10)]}
                 :runner {:deck ["Guru Davinder" (qty "Sure Gamble" 4)]}})
      (play-from-hand state :corp "Obokata Protocol" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :agenda-point 6)
      (play-from-hand state :runner "Guru Davinder")
      (run-empty-server state "Server 1")
      (is (= ["No action"] (prompt-buttons :runner)) "Should only have No action choice")
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-runner)))) "Runner did not pay damage")
      (is (not= :runner (:winner @state)) "Runner has not won")))

(deftest guru-davinder-uses-async-handle-mid-run-steals-5283
    ;; Uses async handle mid-run steals #5283
    (do-game
      (new-game {:corp {:id "Argus Security: Protection Guaranteed"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Vanity Project"]}
                 :runner {:deck ["Guru Davinder"]
                          :credits 100}})
      (take-credits state :corp)
      (play-from-hand state :runner "Guru Davinder")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Suffer 2 meat damage")
      (click-prompt state :runner "Pay 4 [Credits]")
      (is (not (get-run)))))

(deftest guru-davinder-trash-does-not-trigger-dummy-box
    ;; Guru Davinder trash doesn't trigger Dummy Box
    (do-game
      (new-game {:corp {:id "Argus Security: Protection Guaranteed"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Vanity Project"]}
                 :runner {:deck [(qty "Guru Davinder" 2) "Dummy Box"]
                          :credits 100}})
      (take-credits state :corp)
      (play-from-hand state :runner "Guru Davinder")
      (play-from-hand state :runner "Dummy Box")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Suffer 2 meat damage")
      (click-prompt state :runner "Trash Guru Davinder")
      (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trash")))

(deftest hannah-wheels-pilintra-basic-test
  (do-game
    (new-game {:runner {:hand ["Hannah \"Wheels\" Pilintra"]}
               :corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Rashida Jaheem"]}})
    (play-from-hand state :corp "Rashida Jaheem" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Hannah \"Wheels\" Pilintra")
    (let [wheels (get-resource state 0)]
      (is (changed? [(:click (get-runner)) 0]
            (card-ability state :runner (refresh wheels) 0)
            (click-prompt state :runner "Server 1"))
          "Gained 1 click back")
      (is (changed? [(count-tags state) 1]
            (run-jack-out state))
          "Runner took a tag for jacking out")
      (run-on state "Server 1")
      (is (changed? [(count-tags state) 0]
            (run-jack-out state))
          "Runner took no tag for jacking out")
      (take-credits state :runner))))

(deftest hard-at-work
  ;; Hard at Work - Gain 2c and lose 1 click when turn begins
  (do-game
    (new-game {:runner {:deck ["Hard at Work"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Hard at Work")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Gained 2c")
    (is (= 3 (:click (get-runner))) "Lost 1 click")))

(deftest hernando-cortez-rezzing-a-one-subroutine-piece-of-ice
    ;; Rezzing a one subroutine piece of ice
    (do-game
      (new-game {:corp {:deck [(qty "Paper Wall" 3) "Launch Campaign"]}
                 :runner {:deck ["Hernando Cortez"]}})
      (play-from-hand state :corp "Paper Wall" "HQ")
      (play-from-hand state :corp "Paper Wall" "R&D")
      (play-from-hand state :corp "Paper Wall" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Hernando Cortez")
      (take-credits state :runner)
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp 2)
      (is (= 12 (:credit (get-corp))) "Corp should have 12 credits")
      (let [pw1 (get-ice state :hq 0)
            pw2 (get-ice state :rd 0)
            pw3 (get-ice state :archives 0)
            lc (get-content state :remote1 0)]
        (rez state :corp lc)
        (is (= 11 (:credit (get-corp))) "Paid 1 to rez Launch Campaign; no effect on non-ice")
        (rez state :corp pw1)
        (is (= 10 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
        (rez state :corp pw2)
        (is (= 9 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
        (rez state :corp pw3)
        (is (= 9 (:credit (get-corp))) "Paid 0 to rez Paper Wall"))))

(deftest hernando-cortez-rezzing-a-three-subroutine-piece-of-ice
    ;; Rezzing a three subroutine piece of ice
    (do-game
      (new-game {:corp {:deck [(qty "Ichi 1.0" 2) "Launch Campaign"]}
                 :runner {:deck ["Hernando Cortez"]}})
      (play-from-hand state :corp "Ichi 1.0" "HQ")
      (play-from-hand state :corp "Ichi 1.0" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Hernando Cortez")
      (take-credits state :runner)
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 14 (:credit (get-corp))) "Corp should have 14 credits")
      (let [ichi1 (get-ice state :hq 0)
            ichi2 (get-ice state :rd 0)
            lc (get-content state :remote1 0)]
        (rez state :corp lc)
        (is (= 13 (:credit (get-corp))) "Paid 1 to rez Launch Campaign; no effect on non-ice")
        (rez state :corp ichi1)
        (is (= 5 (:credit (get-corp))) "Paid 8 instead of 5 to rez Ichi 1.0")
        (rez state :corp ichi2)
        (is (= 0 (:credit (get-corp))) "Paid 5 to rez Ichi 1.0"))))

(deftest hernando-cortez-rezzing-a-zero-subroutine-piece-of-ice
    ;; Rezzing a zero subroutine piece of ice
    (do-game
      (new-game {:corp {:deck ["Tour Guide" "NEXT Silver" "Launch Campaign"]}
                 :runner {:deck ["Hernando Cortez"]}})
      (play-from-hand state :corp "Tour Guide" "HQ")
      (play-from-hand state :corp "NEXT Silver" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Hernando Cortez")
      (take-credits state :runner)
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp 2)
      (is (= 13 (:credit (get-corp))) "Corp should have 13 credits")
      (let [tour-guide (get-ice state :hq 0)
            next-silver (get-ice state :rd 0)
            lc (get-content state :remote1 0)]
        (rez state :corp lc)
        (is (= 12 (:credit (get-corp))) "Paid 1 to rez Launch Campaign; no effect on non-ice")
        (rez state :corp tour-guide)
        (is (= 10 (:credit (get-corp))) "Paid 2 to rez Tour Guide")
        (rez state :corp next-silver)
        (is (= 7 (:credit (get-corp))) "Paid 3 to rez NEXT Silver"))))

(deftest hernando-cortez-interactions-with-non-rez-abilities-such-as-blue-sun-issue-4000
    ;; interactions with non-rez abilities, such as Blue Sun. Issue #4000
    (do-game
      (new-game {:corp {:id "Blue Sun: Powering the Future"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]
                        :credits 15}
                 :runner {:hand ["Hernando Cortez"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hernando Cortez")
      (let [iw (get-ice state :hq 0)
            credits (:credit (get-corp))]
        (rez state :corp iw)
        (is (= (- credits 2) (:credit (get-corp))) "Corp should pay an addition 1 to rez Ice Wall")
        (take-credits state :runner)
        (card-ability state :corp (:identity (get-corp)) 0)
        (click-card state :corp iw)
        (is (= (dec credits) (:credit (get-corp))) "Corp should only gain 1 back when using Blue Sun's ability"))))

(deftest hunting-grounds-preventing-an-on-encounter-effect
    ;; Preventing an on-encounter effect
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Tollbooth"]
                        :credits 10}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (run-on state "Server 1")
      (let [credits (:credit (get-runner))]
        (card-ability state :runner (get-resource state 0) 0)
        (run-continue state)
        (is (= credits (:credit (get-runner))) "Runner doesn't lose any credits to Tollbooth")
        (is (:run @state) "Run hasn't ended from not paying Tollbooth"))))

(deftest hunting-grounds-preventing-an-on-encounter-effect-auto-fire
    ;; Preventing an on-encounter effect auto-fire
    (do-game
      (new-game {:corp {:hand ["Tollbooth"]
                        :credits 10}
                 :runner {:hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (run-on state "Server 1")
      (let [credits (:credit (get-runner))]
        (run-continue state)
        (click-prompt state :runner "Yes")
        (is (= credits (:credit (get-runner))) "Runner doesn't lose any credits to Tollbooth")
        (is (:run @state) "Run hasn't ended from not paying Tollbooth"))))

(deftest hunting-grounds-preventing-an-on-encounter-effect-auto-fire-choose-not-to-fire
    ;; Preventing an on-encounter effect auto-fire, choose not to fire
    (do-game
      (new-game {:corp {:hand ["Tollbooth"]
                        :credits 10}
                 :runner {:hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (run-on state "Server 1")
      (run-continue state)
      (click-prompt state :runner "No")
      (is (zero? (:credit (get-runner))) "Runner loses credits to Tollbooth")
      (is (:run @state) "Run hasn't ended when paying Tollbooth")))

(deftest hunting-grounds-prints-correctly-to-the-log
    ;; Prints correctly to the log
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Tollbooth"]
                        :credits 10}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (run-on state "Server 1")
      (card-ability state :runner (get-resource state 0) 0)
      (is (last-log-contains? state "prevent the encounter effect on Tollbooth protecting Server 1 at position 0")
          "Hunting Grounds logs the ability")))

(deftest hunting-grounds-only-prevents-the-on-encounter-effects-of-a-single-ice
    ;; Only prevents the on-encounter effects of a single ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Tollbooth" 2)]
                        :credits 20}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (play-from-hand state :corp "Tollbooth" "Server 1")
      (rez state :corp (get-ice state :remote1 0))
      (rez state :corp (get-ice state :remote1 1))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (run-on state "Server 1")
      (let [credits (:credit (get-runner))]
        (card-ability state :runner (get-resource state 0) 0)
        (run-continue state)
        (run-continue-until state :encounter-ice)
        (is (= (- credits 3) (:credit (get-runner))) "Runner loses 3 credits to Tollbooth 2 "))))

(deftest hunting-grounds-only-prevents-the-on-encounter-effects-once-per-turn-issue-4807
    ;; Only prevents the on-encounter effects once per turn. Issue #4807
    (do-game
      (new-game {:corp {:hand ["Tollbooth"]
                        :credits 10}
                 :runner {:hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (let [credits (:credit (get-runner))]
        (run-on state "Server 1")
        (card-ability state :runner (get-resource state 0) 0)
        (run-continue state)
        (is (= credits (:credit (get-runner))) "Runner doesn't lose any credits to Tollbooth")
        (run-continue state :movement)
        (run-jack-out state))
      (let [credits (:credit (get-runner))]
        (run-on state "Server 1")
        (run-continue state)
        (is (= (- credits 3) (:credit (get-runner))) "Runner loses credits to Tollbooth"))))

(deftest hunting-grounds-only-prevents-the-on-encounter-effects-once-per-turn-auto-fire-case
    ;; Only prevents the on-encounter effects once per turn, auto-fire case
    (do-game
      (new-game {:corp {:hand ["Tollbooth"]
                        :credits 10}
                 :runner {:hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (let [credits (:credit (get-runner))]
        (run-on state "Server 1")
        (run-continue state)
        (click-prompt state :runner "Yes")
        (is (= credits (:credit (get-runner))) "Runner doesn't lose any credits to Tollbooth")
        (run-continue state :movement)
        (run-jack-out state))
      (let [credits (:credit (get-runner))]
        (run-on state "Server 1")
        (run-continue state)
        (is (no-prompt? state :runner) "No Hunting Grounds prompt")
        (is (= (- credits 3) (:credit (get-runner))) "Runner loses credits to Tollbooth"))))

(deftest hunting-grounds-only-prevents-the-on-encounter-effects-once-per-turn-mixed-use-case
    ;; Only prevents the on-encounter effects once per turn, mixed-use case
    (do-game
      (new-game {:corp {:hand ["Tollbooth"]
                        :credits 10}
                 :runner {:hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (let [credits (:credit (get-runner))]
        (run-on state "Server 1")
        (card-ability state :runner (get-resource state 0) 0)
        (run-continue state)
        (is (= credits (:credit (get-runner))) "Runner doesn't lose any credits to Tollbooth")
        (run-continue state :movement)
        (run-jack-out state))
      (let [credits (:credit (get-runner))]
        (run-on state "Server 1")
        (run-continue state)
        (is (no-prompt? state :runner) "No Hunting Grounds prompt")
        (is (= (- credits 3) (:credit (get-runner))) "Runner loses credits to Tollbooth"))))

(deftest hunting-grounds-preventing-an-on-encounter-effect-5037
    ;; Preventing an on-encounter effect #5037
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Konjin"]
                        :credits 10}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Hunting Grounds"]}})
      (play-from-hand state :corp "Konjin" "New remote")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hunting Grounds")
      (run-on state "Server 1")
      (card-ability state :runner (get-resource state 0) 0)
      (is (last-log-contains? state "uses Hunting Grounds to prevent the encounter effect on Konjin"))))

(deftest ice-analyzer-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Ice Analyzer" "Equivocation"]}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp 2)
      (let [iwall (get-ice state :archives 0)]
        (play-from-hand state :runner "Ice Analyzer")
        (rez state :corp iwall)
        (let [ana (get-resource state 0)]
          (is (changed? [(:credit (get-runner)) -1]
                (play-from-hand state :runner "Equivocation")
                (click-card state :runner ana))
              "Used 1 credit from Ice Analyzer")))))

(deftest ice-carver
  ;; Ice Carver - lower ice strength on encounter
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Ganked!"]}
               :runner {:deck ["Ice Carver"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ganked!" "Archives")
    (take-credits state :corp 2)
    (let [iwall (get-ice state :archives 0)]
      (play-from-hand state :runner "Ice Carver")
      (run-on state "Archives")
      (rez state :corp iwall)
      (run-continue state)
      (is (zero? (get-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-continue state :movement)
      (is (= 1 (get-strength (refresh iwall))) "Ice Wall strength at 1 after encounter")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp iwall)
      (is (zero? (get-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-continue state)
      (is (= 1 (get-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest info-bounty
    ;; Info Bounty
    (do-game
      (new-game {:corp {:hand ["Hedge Fund"]}
                 :runner {:deck ["Info Bounty" "Patron"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Info Bounty")
      (play-from-hand state :runner "Patron")
      (core/set-mark state :hq)
      (is (changed? [(:credit (get-runner)) 0]
            (run-empty-server state :archives))
          "Gained no credits when running not on mark")
      (is (changed? [(:credit (get-runner)) 2]
            (run-empty-server state :hq)
            (click-prompt state :runner "No action"))
          "Gained 2 credits")
      (is (changed? [(:credit (get-runner)) 0]
            (run-empty-server state :hq)
            (click-prompt state :runner "No action"))
          "Gained no credits on subsequent runs on mark")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Archives")
      (core/set-mark state :archives)
      (is (changed? [(:credit (get-runner)) 0]
            (run-empty-server state :archives))
          "Gained no credits on run on mark when breach is replaced")
      (is (changed? [(:credit (get-runner)) 0]
            (run-empty-server state :archives))
          "Gained no credits on sunsequent runs with breach on mark even if breach didn't happen yet")))

(deftest info-bounty-with-virtuoso
    ;; Info Bounty - interaction with Virtuoso
    (do-game
      (new-game {:corp {:hand ["Hedge Fund"]
                        :deck [(qty "Hedge Fund" 10)]}
                 :runner {:deck ["Info Bounty" "Virtuoso"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Info Bounty")
      (play-from-hand state :runner "Virtuoso")
      (core/set-mark state :rd)
      (is (changed? [(:credit (get-runner)) 2]
            (run-empty-server state :rd)
            (click-prompt state :runner "No action")
            (click-prompt state :runner "Info Bounty"))
          "Gained 2 credits when finished breaching mark")
      (click-prompt state :runner "No action")))

(deftest inside-man-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Inside Man" "Desperado"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Inside Man")
      (let [im (get-resource state 0)]
        (is (changed? [(:credit (get-runner)) -1]
              (play-from-hand state :runner "Desperado")
              (click-card state :runner im)
              (click-card state :runner im))
            "Used 2 credits from Inside Man"))))

(deftest investigative-journalism
  ;; Investigative Journalism - 4 clicks and trash to give the Corp 1 bad pub
  (do-game
    (new-game {:runner {:deck ["Investigative Journalism"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Investigative Journalism")
    (is (empty? (get-resource state)) "Corp has no bad pub, couldn't install")
    (core/gain state :corp :bad-publicity 1)
    (play-from-hand state :runner "Investigative Journalism")
    (take-credits state :runner)
    (take-credits state :corp)
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (:click (get-runner))) "Spent 4 clicks")
    (is (= 1 (count (:discard (get-runner)))) "IJ is trashed")
    (is (= 2 (count-bad-pub state)) "Corp took 1 bad publicity")))

(deftest jackpot
  ;; Jackpot! - whenever a card enters your score area, trash Jackpot to pull off credits
  (do-game
      (new-game {:corp {:deck ["Braintrust"]}
                 :runner {:deck ["Jackpot!"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Jackpot!")
      (let [jak (get-resource state 0)]
        (is (zero? (get-counters (refresh jak) :credit)) "Jackpot! starts with 0 credits")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn (2nd turn)")
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (is (= 2 (:agenda-point (get-runner))) "Runner steals Braintrust")
        (click-prompt state :runner "Yes")
        (is (= 12 (:credit (get-runner))) "Runner starts with 12 credits")
        (click-prompt state :runner "2")
        (is (= 14 (:credit (get-runner))) "Runner gains 2 credits")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed"))))

(deftest jackpot-should-fire-when-moving-agendas-from-film-critic-to-scored-area
    ;; should fire when moving agendas from Film Critic to scored area
    (do-game
      (new-game {:corp {:deck ["Project Vitruvius"]}
                 :runner {:deck ["Jackpot!" "Film Critic"]}})
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Film Critic")
      (play-from-hand state :runner "Jackpot!")
      (let [fc (get-resource state 0)]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :runner)
        (take-credits state :corp)
        (card-ability state :runner fc 0)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "1")
        (is (= 1 (count (:scored (get-runner)))) "Moved agenda to scored area")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed")
        (is (empty? (:hosted (refresh fc))) "Removed agenda hosted on FC"))))

(deftest jackpot-should-fire-when-trashing-chairman-hiro
    ;; should fire when trashing Chairman Hiro
    (do-game
      (new-game {:corp {:deck ["Chairman Hiro"]}
                 :runner {:deck ["Jackpot!"]}})
      (play-from-hand state :corp "Chairman Hiro" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Jackpot!")
      (let [jak (get-resource state 0)]
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 6 [Credits] to trash") ;trash CH
        (click-prompt state :runner "Yes") ;trash Jackpot!
        (click-prompt state :runner "1")
        (is (= 3 (:credit (get-runner))) "Runner gains 1 credit")
        (is (= 1 (count (:scored (get-runner)))) "Chairman Hiro in score area")
        (is (= 1 (count (:discard (get-runner)))) "Jackpot! trashed"))))

(deftest jak-sinclair-lost-clicks-carry-through-to-when-turn-starts-fully-1764
    ;; Lost clicks carry through to when turn starts fully #1764
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3)]}
                 :runner {:deck [(qty "Jak Sinclair" 3)]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Jak Sinclair")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [eni (get-ice state :hq 0)
            jak (get-resource state 0)]
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (card-ability state :runner jak 0)
        (click-prompt state :runner "HQ")
        (rez state :corp eni)
        (run-continue state)
        (card-subroutine state :corp (refresh eni) 0)
        (run-continue-until state :success)
        (click-prompt state :runner "No action")
        (end-phase-12 state :runner)
        (is (= 3 (:click (get-runner))) "Enigma took a click")
        (is (no-prompt? state :runner) "Jak Sinclair did not trigger at :runner-turn-begins"))))

(deftest jak-sinclair-calls-make-run-async-5612
    ;; Calls make-run async #5612
    (do-game
      (new-game {:corp {:hand ["Hedge Fund"]
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Sunny Lebeau: Security Specialist"
                          :hand ["Jak Sinclair" (qty "Data Folding" 3) "DreamNet" "The Archivist"]
                          :credits 100}})
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Jak Sinclair")
      (play-from-hand state :runner "Data Folding")
      (play-from-hand state :runner "Data Folding")
      (play-from-hand state :runner "Data Folding")
      (play-from-hand state :runner "DreamNet")
      (play-from-hand state :runner "The Archivist")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (:runner-phase-12 @state) "Runner in Step 1.2")
      (end-phase-12 state :runner)
      (click-prompt state :runner "Data Folding")
      (click-prompt state :runner "Jak Sinclair")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "R&D")
      (run-continue state :success)
      (is (accessing state "Hedge Fund"))
      (click-prompt state :runner "No action")
      (is (not (get-run)) "Run has ended")
      (is (no-prompt? state :corp) "Corp does not have a start of turn wait prompt")))

(deftest john-masanori-crisium-grid-interaction
    ;; Crisium grid interaction
    (do-game
     (new-game {:corp {:deck ["Crisium Grid"]}
                :runner {:deck [(qty "John Masanori" 3)
                                (qty "Sure Gamble" 3)
                                "Fall Guy"]}})
     (play-from-hand state :corp "Crisium Grid" "HQ")
     (rez state :corp (get-content state :hq 0))
     (take-credits state :corp)
     (core/gain state :runner :click 2 :credit 2)
     (play-from-hand state :runner "John Masanori")
     (is (= 4 (count (:hand (get-runner)))))
     (run-empty-server state "HQ")
     (click-prompt state :runner "Pay 5 [Credits] to trash") ; trash crisium #2433
     (run-empty-server state "Archives")
     (is (= 5 (count (:hand (get-runner)))) "1 card drawn from first successful run")
     (run-empty-server state "Archives")
     (is (= 5 (count (:hand (get-runner)))) "No card drawn from second successful run")
     (run-on state "HQ")
     (run-jack-out state)
     (is (= 1 (count-tags state)) "1 tag taken from first unsuccessful run")
     (run-on state "HQ")
     (run-jack-out state)
     (is (= 1 (count-tags state)) "No tag taken from second unsuccessful run")))

(deftest john-masanori-masanori-paragon-interaction-should-get-order-of-choice
    ;; Masanori+Paragon interaction - should get order of choice
    (do-game
     (new-game {:runner {:deck ["Paragon" "John Masanori" "Sure Gamble" "Easy Mark"]}})
     (take-credits state :corp)
     (core/move state :runner (find-card "Easy Mark" (:hand (get-runner))) :deck)
     (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
     (is (= "Easy Mark" (:title (first (:deck (get-runner)))))
         "Easy Mark on top of stack")
     (is (= "Sure Gamble" (:title  (second (:deck (get-runner)))))
         "Sure Gamble on bottom of stack")
     (core/gain state :runner :credit 10)
     (play-from-hand state :runner "Paragon")
     (play-from-hand state :runner "John Masanori")
     (run-empty-server state :rd)
     (click-prompt state :runner "Paragon") ; runner should be prompted for which to trigger first
     (is (= "Gain 1 [Credit] and look at the top card of the stack?" (:msg (prompt-map :runner))) "Paragon prompt 1")
     (click-prompt state :runner "Yes")
     (is (= "Add Easy Mark to bottom of the stack?" (:msg (prompt-map :runner))) "Paragon prompt")
     (is (changed? [(count (:hand (get-runner))) 1]
           (click-prompt state :runner "Yes"))
         "Clicking prompt causes Masanori to resolve")
     (is (= "Easy Mark" (:title (last (:deck (get-runner)))))
         "Easy Mark on bottom of stack")))

(deftest joshua-b
  ;; Joshua B. - Take 1 tag at turn end if you choose to gain the extra click
  (do-game
    (new-game {:runner {:deck ["Joshua B."]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Joshua B.")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (is (:runner-phase-12 @state) "Runner is in Step 1.2")
    (end-phase-12 state :runner)
    (click-prompt state :runner "Yes")
    (is (= 5 (:click (get-runner))) "Gained extra click from Joshua")
    (is (zero? (count-tags state)) "Runner has no tags during turn")
    (take-credits state :runner)
    (is (= 1 (count-tags state)) "Took 1 tag")
    (take-credits state :corp)
    (end-phase-12 state :runner)
    (click-prompt state :runner "No")
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (remove-tag state :runner)
    (take-credits state :runner)
    (is (zero? (count-tags state)) "Runner has no tags during turn")))

(deftest juli-moreira-lee
  (do-game
    (new-game {:runner {:hand ["Juli Moreira Lee" "The Artist"]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Artist")
    (play-from-hand state :runner "Juli Moreira Lee")
    (let [artist (get-resource state 0)
          juli (get-resource state 1)]
      (is (changed? [(get-counters (refresh juli) :power) -1
                     (:click (get-runner)) 0]
                    (card-ability state :runner artist 0))
          "Runner gained 1 click from Juli Moreira Lee")
      (is (changed? [(get-counters (refresh juli) :power) 0]
                    (card-ability state :runner artist 1))
          "No further Juli Moreira Lee trigger")
      (take-credits state :runner)
      (take-credits state :corp)
      (core/add-counter state :runner (refresh juli) :power -2)
      (card-ability state :runner artist 0)
      (is (= 1 (count (:discard (get-runner)))) "Juli Moreira Lee trashed"))))

(deftest kasi-string
  ;; Kasi String
  (do-game
      (new-game {:corp {:deck ["NGO Front"]}
                 :runner {:deck ["Kasi String"]}})
      (play-from-hand state :corp "NGO Front" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Kasi String")
      ;; using autoresolve
      (let [kasi (get-resource state 0)]
        (card-ability state :runner kasi 0)
        (click-prompt state :runner "Always")
        (dotimes [_ 4]
          (run-empty-server state "Server 1")
          (click-prompt state :runner "No action")
          (take-credits state :runner)
          (take-credits state :corp))
        (is (= 1 (count (:scored (get-runner)))) "Kasi String moved to score area"))))

(deftest kasi-string-when-charged-becomes-an-agenda
  ;; Kasi String becomes an agenda when charged up to 4 counters
  (do-game
      (new-game {:corp {:deck ["NGO Front" "15 Minutes"]}
                 :runner {:deck ["Kasi String" "Daeg, First Net-Cat"]}})
      (play-from-hand state :corp "NGO Front" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Daeg, First Net-Cat")
      (play-from-hand state :runner "Kasi String")
      (dotimes [_ 3]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Yes")
        (take-credits state :runner)
        (take-credits state :corp))
      (take-credits state :runner)
      (play-and-score state "15 Minutes")
      ;; Make Daeg charge Kasi String
      (click-card state :runner "Kasi String")
      (is (= 1 (count (:scored (get-runner)))) "Kasi String moved to score area")))

(deftest kasi-string-no-counter-when-stealing-agenda
    ;; No counter when stealing agenda
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Kasi String"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Kasi String")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (no-prompt? state :runner) "Kasi String shouldn't display a prompt")))

(deftest kasi-string-triggers-only-on-remote-server
    ;; Triggers only on remote server
    (do-game
      (new-game {:corp {:deck ["NGO Front" "Hedge Fund"]}
                 :runner {:deck ["Kasi String"]}})
      (play-from-hand state :corp "NGO Front" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Kasi String")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (zero? (get-counters (get-resource state 0) :power))
          "Kasi String should have 0 power counter on itself - no trigger on HQ")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= 1 (get-counters (get-resource state 0) :power)) "Kasi String should have 1 power counter on itself")))

(deftest kati-jones
  ;; Kati Jones - Click to store and take
  (do-game
    (new-game {:runner {:deck ["Kati Jones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (is (= 3 (:credit (get-runner))))
    (let [kati (get-resource state 0)]
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))))
      (is (= 3 (get-counters (refresh kati) :credit)) "Store 3cr on Kati")
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))) "Second use of Kati should not be allowed")
      (is (= 3 (get-counters (refresh kati) :credit)) "Second use of Kati should not be allowed")
      (take-credits state :runner 2)
      (is (= 5 (:credit (get-runner))) "Pass turn, take 2cr")
      (take-credits state :corp)
      (card-ability state :runner kati 0)
      (is (= 6 (get-counters (refresh kati) :credit)) "Store 3cr more on Kati")
      (take-credits state :runner 3)
      (is (= 8 (:credit (get-runner))) "Pass turn, take 3cr")
      (take-credits state :corp)
      (card-ability state :runner (refresh kati) 1)
      (is (= 14 (:credit (get-runner))) "Take 6cr from Kati")
      (is (zero? (get-counters (refresh kati) :credit)) "No counters left on Kati"))))

(deftest keros-mcintyre
  ;; Keros Mcintyre
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma" "Chimera"]
                      :credits 100}
               :runner {:hand ["Keros Mcintyre" (qty "Emergency Shutdown" 2)]
                        :credits 100}})
    (play-from-hand state :corp "Enigma" "New remote")
    (play-from-hand state :corp "Chimera" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Keros Mcintyre")
    (take-credits state :runner)
    (let [enigma (get-ice state :remote1 0)
          chimera (get-ice state :remote2 0)]
      (rez state :corp chimera)
      (click-prompt state :corp "Code Gate")
      (is (changed? [(:credit (get-runner)) 0]
            (take-credits state :corp))
          "No credit gain from corp derezzing a card")
      (rez state :corp chimera)
      (click-prompt state :corp "Code Gate")
      (rez state :corp enigma)
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (changed? [(:credit (get-runner)) 2]
            (play-from-hand state :runner "Emergency Shutdown")
            (click-card state :runner "Enigma"))
          "Credit gain from runner derezzing a card")
      (is (changed? [(:credit (get-runner)) 0]
            (play-from-hand state :runner "Emergency Shutdown")
            (click-card state :runner "Chimera"))
          "No credit gain a second time"))))

(deftest kongamato
  ;; Kongamato
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma"]}
               :runner {:hand [(qty "Kongamato" 2)]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Kongamato")
    (play-from-hand state :runner "Kongamato")
    (run-on state :hq)
    (run-continue state)
    (let [kongamato-1 (get-resource state 0)
          kongamato-2 (get-resource state 1)
          enigma (get-ice state :hq 0)]
      (card-ability state :runner (refresh kongamato-1) 0)
      (is (nil? (refresh kongamato-1)) "Kongamato is trashed")
      (is (:broken (first (:subroutines (refresh enigma))))
          "Enigma's Lose a Click should be broken")
      (is (not (:broken (last (:subroutines (refresh enigma)))))
          "Enigma's ETR should not be broken")
      (card-ability state :runner (refresh kongamato-2) 0)
      (is (refresh kongamato-2)
          "Second Kongamato isn't trashed as first subroutine is already broken"))))

(deftest lago-paranoa-shelter
  (do-game
      (new-game {:runner {:hand ["Lago Paranoá Shelter"]
                          :deck [(qty "Sure Gamble" 3)]}
                 :corp {:hand ["PAD Campaign" "Crisium Grid" "Ice Wall" "Spin Doctor" "Eavesdrop"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Lago Paranoá Shelter")
      (take-credits state :runner)
      (core/gain-clicks state :corp 1)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (no-prompt? state :runner) "Ice don't trigger Lago Paranoá Shelter")
      (play-from-hand state :corp "Eavesdrop")
      (click-card state :corp "Ice Wall")
      (is (no-prompt? state :runner) "Condition counters don't trigger Lago Paranoá Shelter")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (is (changed? [(count (:discard (get-runner))) 1]
            (click-prompt state :runner "Yes"))
          "Milled 1 card")
      (is (= 1 (count (:hand (get-runner)))) "Runner drew 1 card")
      (play-from-hand state :corp "Spin Doctor" "New remote")
      (is (no-prompt? state :runner) "Lago Paranoá Shelter is once per turn")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (is (changed? [(count (:discard (get-runner))) 1]
            (click-prompt state :runner "Yes"))
          "Milled 1 card")
      (is (= 1 (count (:hand (get-runner)))) "No cards left to draw")))

(deftest levy-advanced-research-lab
  (do-game
      (new-game {:runner {:hand ["Levy Advanced Research Lab"]
                          :deck [(qty "Sure Gamble" 3) "Ika"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Levy Advanced Research Lab")
      (card-ability state :runner (get-resource state 0) 0)
      (is (last-log-contains? state "Sure Gamble"))
      (is (last-log-contains? state "Ika"))
      (click-prompt state :runner "Ika")
      (click-prompt state :runner "Sure Gamble")
      (click-prompt state :runner "Sure Gamble")
      (click-prompt state :runner "Sure Gamble")
      (is (find-card "Ika" (:hand (get-runner))) "Ika was added to the grip")))

(deftest lewi-guilherme
  ;; Lewi Guilherme - lower corp hand size by 1, pay 1 credit when turn begins or trash
  (do-game
    (new-game {:runner {:deck [(qty "Lewi Guilherme" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Lewi Guilherme")
    (is (= 4 (hand-size :corp)) "Corp hand size reduced by 1")
    (take-credits state :runner)
    (core/lose state :runner :credit 6)
    (is (= 2 (:credit (get-runner))) "Credits are 2")
    (take-credits state :corp)
    (click-prompt state :runner "Pay 1 [Credits]")
    (is (= 1 (:credit (get-runner))) "Lost a credit from Lewi")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "Trash Lewi Guilherme")
    (is (= 1 (count (:discard (get-runner)))) "First Lewi trashed")
    (is (= 5 (hand-size :corp)) "Corp hand size normal again")
    (play-from-hand state :runner "Lewi Guilherme")
    (take-credits state :runner)
    (core/lose state :runner :credit 8)
    (is (zero? (:credit (get-runner))) "Credits are 0")
    (take-credits state :corp)
    (is (= ["Trash Lewi Guilherme"] (prompt-buttons :runner)) "Runner has no longer the option to lose credits")
    (click-prompt state :runner "Trash Lewi Guilherme")
    (is (= 2 (count (:discard (get-runner)))) "Second Lewi trashed due to no credits")))

(deftest lewi-guilherme-lovegood-interaction-#3345
  ;; hand size does not persist while blanked by Dr. Lovegood
  (do-game
   (new-game {:runner {:hand ["Lewi Guilherme" "Dr. Lovegood"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Lewi Guilherme")
   (is (= 4 (hand-size :corp)) "-1 hand size from lewi")
   (play-from-hand state :runner "Dr. Lovegood")
   (take-credits state :runner)
   (take-credits state :corp)
   (click-prompt state :runner "Dr. Lovegood")
   (click-card state :runner "Lewi Guilherme")
   (is (= 5 (hand-size :corp)) "regular hand size from lewi")
   (is (no-prompt? state :runner) "No more prompt to activate")))

(deftest liberated-account
  ;; Liberated Account
  (do-game
      (new-game {:runner {:deck ["Liberated Account"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 1)
      (play-from-hand state :runner "Liberated Account")
      (is (changed? [(:credit (get-runner)) 4]
            (card-ability state :runner (get-resource state 0) 0))
          "Gained 4 credits")
      (core/gain state :runner :click 4)
      (dotimes [_ 3]
        (card-ability state :runner (get-resource state 0) 0))
      (is (= 1 (count (:discard (get-runner)))) "Liberated Account trashed")))

(deftest light-the-fire-happy-path
  ;; Light the Fire!
  (do-game
   (new-game {:corp {:hand ["Hokusai Grid"]}
              :runner {:hand ["Light the Fire!", "Sure Gamble", "Sure Gamble"]}})
   (play-from-hand state :corp "Hokusai Grid" "New remote")
   (take-credits state :corp)
   (play-from-hand state :runner "Light the Fire!")
   (card-ability state :runner (get-resource state 0) 0)
   (click-prompt state :runner "Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Lost card from Grip to core damage")
   (is (= 1 (:brain-damage (get-runner))))
   (rez state :corp (refresh (get-content state :remote1 0)))
   (is (changed? [(:credit (get-runner)) 0]
         (run-continue state)
         (is (= 1 (count (:discard (get-corp)))) "Hokusai Grid trashed from Server 1"))
       "Did not spend credits to trash")
   (is (nil? (get-in @state [:corp :servers :remote1 :content])) "Server 1 no longer exists")
   (is (= 1 (count (:hand (get-runner)))) "Did not take damage from Hokusai Grid")))

(deftest light-the-fire-effect-goes-away
  ;; Light the Fire - effect goes away after end of run
  (do-game
   (new-game {:corp {:hand ["Hokusai Grid"]}
              :runner {:hand ["Light the Fire!", "Sure Gamble", "Sure Gamble"]}})
   (play-from-hand state :corp "Hokusai Grid" "New remote")
   (take-credits state :corp)
   (play-from-hand state :runner "Light the Fire!")
   (card-ability state :runner (get-resource state 0) 0)
   (click-prompt state :runner "Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Lost card from Grip to core damage")
   (is (= 1 (:brain-damage (get-runner))))
   (rez state :corp (refresh (get-content state :remote1 0)))
   (run-jack-out state)
   (is (not (nil? (get-in @state [:corp :servers :remote1 :content]))) "Server 1 still exists")
   (run-empty-server state "Server 1")
   (click-prompt state :runner "No action")
   (is (= 0 (count (:hand (get-runner)))) "Lost card from Grip to Hokusai Grid")))

(deftest light-the-fire-card-installed
  ;; Light the fire - effect applies if the corporation installs a card mid run
  (do-game
   (new-game {:corp {:hand ["Crick"] :discard ["Hokusai Grid"]}
              :runner {:hand ["Sure Gamble" "Sure Gamble" "Light the Fire!"]}})
   (play-from-hand state :corp "Crick" "New remote")
   (take-credits state :corp)
   (play-from-hand state :runner "Light the Fire!")
   (card-ability state :runner (get-resource state 0) 0)
   (click-prompt state :runner "Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Lost card from Grip to core damage")
   (let [crick (get-ice state :remote1 0)]
     (rez state :corp crick)
     (run-continue state)
     (card-subroutine state :corp crick 0)
     (click-card state :corp "Hokusai Grid")
     (click-prompt state :corp "Server 1"))
   (rez state :corp (refresh (get-content state :remote1 0)))
   (run-continue state :movement)
   (run-continue-until state :success)
   (is (= 1 (count (:discard (get-corp)))) "Hokusai grid trashed from Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Lost no card from Grip to Hokusai Grid")))

(deftest light-the-fire-cards-swapped
  ;; Light the Fire - effect applies to cards swapped into,
  ;; and stops applying to cards swapped out of, the server
  (do-game
   (new-game {:corp {:hand ["Metamorph", "NGO Front", "Hokusai Grid"]
                     :credit 10}
              :runner {:hand ["Light the Fire!" "Sure Gamble" "Sure Gamble"]}})
   (core/gain-clicks state :corp 5)
   (play-from-hand state :corp "NGO Front" "New remote")
   (play-from-hand state :corp "Metamorph" "Server 1")
   (play-from-hand state :corp "Hokusai Grid" "New remote")
   ;(let [ht (get-content state :remote2 0)]
   (click-advance state :corp (refresh (get-content state :remote1 0)))
   (take-credits state :corp)
   (play-from-hand state :runner "Light the Fire!")
   (card-ability state :runner (get-resource state 0) 0)
   (click-prompt state :runner "Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Lost card from Grip to core damage")
   (rez state :corp (refresh (get-content state :remote1 0)))
   (rez state :corp (refresh (get-content state :remote2 0)))
   (let [meta (get-ice state :remote1 0)]
     (rez state :corp meta)
     (run-continue state)
     (fire-subs state (get-ice state :remote1 0))
     (click-prompt state :corp "Swap 2 non-ice")
     (click-card state :corp "NGO Front")
     (click-card state :corp "Hokusai Grid")
     ;; hokusai is now in server 1, ngo in server 2
     (is (changed? [(:credit (get-corp)) 5]
           (card-ability state :corp (refresh (get-content state :remote2 0)) 0))
         "NGO Front reactivated"))
   (run-continue state :movement)
   (run-continue-until state :success)
   (is (= 2 (count (:discard (get-corp)))) "Hokusai grid trashed from Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Lost no card from Grip to Hokusai Grid")))

(deftest light-the-fire-run-redirected
  ;; Light the Fire - effect applies to cards swapped into,
  ;; and stops applying to cards swapped out of, the server
  (do-game
   (new-game {:corp {:hand ["Sand Storm", "NGO Front", "Hokusai Grid"]
                     :credit 10}
              :runner {:hand ["Light the Fire!" "Sure Gamble" "Sure Gamble"]}})
   (core/gain-clicks state :corp 5)
   (play-from-hand state :corp "NGO Front" "New remote")
   (play-from-hand state :corp "Sand Storm" "Server 1")
   (play-from-hand state :corp "Hokusai Grid" "New remote")
   (click-advance state :corp (refresh (get-content state :remote1 0)))
   (take-credits state :corp)
   (play-from-hand state :runner "Light the Fire!")
   (card-ability state :runner (get-resource state 0) 0)
   (click-prompt state :runner "Server 1")
   (is (= 1 (count (:hand (get-runner)))) "Lost card from Grip to core damage")
   (let [sand (get-ice state :remote1 0)
         ngo (get-content state :remote1 0)
         hoku (get-content state :remote2 0)]
     (rez state :corp (refresh ngo))
     (rez state :corp (refresh hoku))
     (rez state :corp sand)
     (run-continue state)
     (fire-subs state (refresh sand))
     (click-prompt state :corp "Server 2")
     (is (changed? [(:credit (get-corp)) 5]
           (card-ability state :corp (refresh ngo) 0))
         "NGO Front reactivated"))
   (is (= :remote2 (first (get-in @state [:run :server]))) "Is running on server 2")
   (run-continue-until state :success)
   (is (= 3 (count (:discard (get-corp)))) "Hokusai grid trashed from Server 2")
   (is (= 1 (count (:hand (get-runner)))) "Lost no card from Grip to Hokusai Grid")))

(deftest light-the-fire-reduced-service-issue-#6340
  ;; github issue #6340
  (do-game
    ;; can afford to pay 8 - pay 8 before the run initiates
    (new-game {:corp {:hand ["Reduced Service"]} :credits 10
               :runner {:hand ["Light the Fire!" "Sure Gamble"]
                        :credits 12}})
    (play-from-hand state :corp "Reduced Service" "New remote")
    (let [rs (get-content state :remote1 0)]
      (rez state :corp rs)
      (click-prompt state :corp "4")
      (take-credits state :corp)
      (play-from-hand state :runner "Light the Fire!")
      (card-ability state :runner (get-resource state 0) 0)
      (is (changed? [(:credit (get-runner)) -8]
            (click-prompt state :runner "Server 1"))
          "Spent 8 credits to make a run on the server"))))

(deftest logic-bomb
  ;; Logic Bomb
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Logic Bomb"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Logic Bomb")
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= 2 (:click (get-runner))) "Should still have 2 clicks")
      (card-ability state :runner (get-resource state 0) 0)
      (is (zero? (:click (get-runner))) "Should now have 0 clicks")
      (is (= 1 (count (:discard (get-runner)))) "Logic Bomb should be discarded")
      (is (last-log-contains? state "use Logic Bomb"))
      (is (last-log-contains? state "[Click][Click]") "Log should mention 2 clicks")))

(deftest logic-bomb-if-the-runner-has-no-clicks-left
    ;; if the runner has no clicks left
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Logic Bomb"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Logic Bomb")
      (click-credit state :runner)
      (click-credit state :runner)
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (zero? (:click (get-runner))) "Should have 0 clicks")
      (card-ability state :runner (get-resource state 0) 0)
      (is (zero? (:click (get-runner))) "Should still have 0 clicks")
      (is (= 1 (count (:discard (get-runner)))) "Logic Bomb should be discarded")
      (is (last-log-contains? state "use Logic Bomb"))
      (is (not (last-log-contains? state "[Click]")) "Log shouldn't mention any clicks")))

(deftest london-library
  ;; Install non-virus programs on London library. Includes #325/409
  (do-game
    (new-game {:runner {:deck ["London Library" "Darwin" "Study Guide"
                               "Chameleon" "Femme Fatale"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "London Library")
    (let [lib (get-resource state 0)]
      (is (zero? (count (:hosted (refresh lib)))) "0 programs hosted")
      (card-ability state :runner lib 0) ; Install a non-virus program on London Library
      (click-card state :runner (find-card "Femme Fatale" (:hand (get-runner))))
      (click-prompt state :runner "Done") ; Cancel out of Femme's bypass
      (is (= 1 (count (:hosted (refresh lib)))) "1 program hosted")
      (card-ability state :runner lib 0)
      (click-card state :runner (find-card "Study Guide" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (let [sg (second (:hosted (refresh lib)))]
        (is (zero? (get-strength (refresh sg))) "Study Guide at 0 strength")
        (card-ability state :runner sg 1) ; Place 1 power counter
        (is (= 1 (get-strength (refresh sg))) "Study Guide at 1 strength"))
      (card-ability state :runner lib 0)
      (click-card state :runner (find-card "Chameleon" (:hand (get-runner))))
      (click-prompt state :runner "Sentry")
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (= 2 (:click (get-runner))) "At 2 clicks")
      (card-ability state :runner lib 0)
      (click-card state :runner (find-card "Darwin" (:hand (get-runner)))) ; Darwin is a virus
      (is (= 3 (count (:hosted (refresh lib)))) "Still 3 programs hosted")
      (is (= 2 (:click (get-runner))) "Failed Darwin didn't use a click")
      (is (= 1 (count (:hand (get-runner)))))
      (click-prompt state :runner "Done")
      (card-ability state :runner lib 1) ; Add a program hosted on London Library to the grip
      (click-card state :runner (find-card "Study Guide" (:hosted (refresh lib))))
      (is (= 2 (count (:hand (get-runner)))) "Return Study Guide to hand")
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (card-ability state :runner lib 0)
      (click-card state :runner (find-card "Study Guide" (:hand (get-runner))))
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (zero? (count (:discard (get-runner)))) "Nothing in archives yet")
      (take-credits state :runner)
      (click-prompt state :runner "Chameleon") ; Simultaneous resolution
      (is (zero? (count (:hosted (refresh lib)))) "All programs trashed when turn ends")
      (is (= 2 (count (:hand (get-runner)))) "Darwin never got played, Chameleon returned to hand")
      (is (= 2 (count (:discard (get-runner)))) "Femme Fatale and Study Guide trashed"))))

(deftest manuel-lattes-de-moura
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 5)]
                      :deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Manuel Lattes de Moura"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Manuel Lattes de Moura")
    (gain-tags state :runner 1)
    (run-empty-server state :rd)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest manuel-lattes-de-moura-threat
  (do-game
    (new-game {:corp {:credits 10 :hand ["Obokata Protocol" (qty "Hedge Fund" 3)]}
               :runner {:hand [(qty "Manuel Lattes de Moura" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Manuel Lattes de Moura")
    (gain-tags state :runner 1)
    (take-credits state :runner)
    (trash-resource state)
    (click-card state :corp (get-resource state 0))
    (is (no-prompt? state :corp) "Manuel threat not yet met")
    (play-and-score state "Obokata Protocol")
    (take-credits state :corp)
    (play-from-hand state :runner "Manuel Lattes de Moura")
    (take-credits state :runner)
    (is (changed? [(count (:hand (get-corp))) -1]
                  (trash-resource state)
                  (click-card state :corp (get-resource state 0))
                  (click-prompt state :corp "Trash 1 card from your hand")
                  (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp)))))
        "Manuel threat active, Corp must trash a card from HQ to trash Manuel")))

(deftest mary-da-silva
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Hedge Fund"]}
               :runner {:hand ["\"Pretty\" Mary da Silva" (qty "Jailbreak" 3)]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "\"Pretty\" Mary da Silva")
    ;; Mary triggers multiple times during the same turn
    (dotimes [_ 2]
      (play-from-hand state :runner "Jailbreak")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (click-prompt state :runner "Jailbreak")
      (click-prompt state :runner "Yes")
      (dotimes [_ 3]
        (click-prompt state :runner "No action")))
    ;; Decline Mary's ability
    (play-from-hand state :runner "Jailbreak")
    (click-prompt state :runner "R&D")
    (run-continue state)
    (click-prompt state :runner "Jailbreak")
    (click-prompt state :runner "No")
    (dotimes [_ 2]
      (click-prompt state :runner "No action"))))

(deftest mary-da-silva-no-trigger-on-single-access
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Hedge Fund"]}
               :runner {:hand ["\"Pretty\" Mary da Silva"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "\"Pretty\" Mary da Silva")
    (run-empty-server state :rd)
    (click-prompt state :runner "No action")))

(deftest miss-bones-can-be-used-mid-run-in-a-trash-prompt
    ;; Can be used mid-run in a trash-prompt
    (do-game
      (new-game {:corp {:hand ["Broadcast Square"]
                        :deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Miss Bones"]}})
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Miss Bones")
      (let [bs (get-content state :remote1 0)
            mb (get-resource state 0)]
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 5 [Credits] to trash")
        (dotimes [_ 5]
          (click-card state :runner "Miss Bones"))
        (is (= 7 (get-counters (refresh mb) :credit)) "Miss Bones loses 5 credits")
        (is (nil? (refresh bs)) "Broadcast Square has been trashed"))))

(deftest miss-bones-can-be-used-outside-of-a-trash-prompt-to-gain-credits-manually
    ;; Can be used outside of a trash-prompt to gain credits manually
    (do-game
      (new-game {:runner {:hand ["Miss Bones"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Miss Bones")
      (let [mb (get-resource state 0)]
        (card-ability state :runner mb 0)
        (click-prompt state :runner "5")
        (is (= 7 (get-counters (refresh mb) :credit)) "Miss Bones loses 5 credits")
        (is (= 8 (get-in @state [:runner :credit]))))))

(deftest miss-bones-can-t-take-more-credits-than-there-are-on-miss-bones
    ;; Can't take more credits than there are on Miss Bones
    (do-game
      (new-game {:runner {:hand ["Miss Bones"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Miss Bones")
      (let [mb (get-resource state 0)]
        (card-ability state :runner mb 0)
        (is (= 12 (get-in (prompt-map :runner) [:choices :number]))
            "Can take up to 12 credits from a newly installed Miss Bones")
        (click-prompt state :runner "5")
        (card-ability state :runner mb 0)
        (is (= 7 (get-in (prompt-map :runner) [:choices :number]))
            "Number of credits that may be taken is reduced after taking money from Miss Bones"))))

(deftest muertos-gang-member
  ;; Muertos Gang Member - Install and Trash
  (do-game
      (new-game {:corp {:deck ["Tollbooth" "Ice Wall"]}
                 :runner {:deck ["Sure Gamble"]
                          :hand [(qty "Sure Gamble" 2) "Muertos Gang Member"]}})
      (play-from-hand state :corp "Tollbooth" "HQ")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (let [toll (get-ice state :hq 0)
            iw (get-ice state :archives 0)]
        (rez state :corp iw)
        (play-from-hand state :runner "Muertos Gang Member")
        (click-card state :corp (refresh iw))
        (is (not (rezzed? (refresh iw))) "Ice Wall derezzed")
        (is (= 2 (count (:hand (get-runner)))) "2 cards in Runner's hand")
        (let [muer (get-resource state 0)]
          (card-ability state :runner muer 0)
          (is (= 3 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
          (click-card state :corp toll)
          (is (rezzed? (refresh toll)) "Tollbooth was rezzed")))))

(deftest muertos-gang-member-account-for-reina-interaction-1098
    ;; Account for Reina interaction, #1098
    (do-game
      (new-game {:corp {:deck ["Tollbooth" "Ice Wall"]}
                 :runner {:id "Reina Roja: Freedom Fighter"
                          :deck [(qty "Sure Gamble" 3)]
                          :hand ["Muertos Gang Member"]}})
      (play-from-hand state :corp "Tollbooth" "HQ")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (let [toll (get-ice state :hq 0)
            iw (get-ice state :archives 0)]
        (rez state :corp iw)
        (take-credits state :corp)
        (core/lose state :corp :credit 100)
        (core/move state :runner (find-card "Hedge Fund" (:hand (get-runner))) :deck)
        (play-from-hand state :runner "Muertos Gang Member")
        (click-card state :corp (refresh iw))
        (is (not (rezzed? (refresh iw))) "Ice Wall derezzed")
        (is (zero? (count (:hand (get-runner)))) "0 cards in Runner's hand")
        (let [muer (get-resource state 0)]
          (card-ability state :runner muer 0)
          (is (= 1 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
          (click-card state :corp toll)
          (is (rezzed? (refresh toll)) "Tollbooth was rezzed")
          (is (zero? (:credit (get-corp))) "Corp has 0 credits")))))

(deftest mystic-maemi
  (do-game
      (new-game {:corp {:deck ["Project Vitruvius"]}
                 :runner {:deck [(qty "Sure Gamble" 3) "Mystic Maemi"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Mystic Maemi")
      (let [mm (get-resource state 0)]
        (is (= 0 (get-counters (refresh mm) :credit)) "No credits on Maemi yet")
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (is (= 1 (get-counters (refresh mm) :credit)) "+1c from steal")
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh mm) :credit)) "+1c from start of turn")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh mm) :credit)) "+1c from start of turn")
        (take-credits state :runner)
        (is (changed? [(count (:hand (get-runner))) -1]
              (click-prompt state :runner "Trash a random card from the grip"))
            "Trashed one card from grip")
        (take-credits state :corp)
        (play-from-hand state :runner "Sure Gamble")
        (is (changed? [(:credit (get-runner)) 5]
              (click-card state :runner mm)
              (click-prompt state :runner "Done"))
            "Used 1 credit from Maemi")
        (take-credits state :runner)
        (is (changed? [(count (:discard (get-runner))) 1]
              (click-prompt state :runner "Trash Mystic Maemi"))
            "Trashed Mystic Maemi"))))

(deftest net-mercur
  ;; Net Mercur - Gains 1 credit or draw 1 card when a stealth credit is used
  (do-game
      (new-game {:runner {:deck ["Net Mercur" "Silencer" "Ghost Runner"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 4 :credit 10)
      (play-from-hand state :runner "Silencer")
      (play-from-hand state :runner "Net Mercur")
      (play-from-hand state :runner "Ghost Runner")
      (let [sil (get-hardware state 0)
            nm (get-resource state 0)
            gr (get-resource state 1)]
        (card-ability state :runner nm 0)
        (is (no-prompt? state :runner) "Net Mercur doesn't stack overflow if you click on it")
        (card-ability state :runner gr 0)
        (is (no-prompt? state :runner) "No Net Mercur prompt from stealth spent outside of run")
        (run-on state :hq)
        (card-ability state :runner sil 0)
        (click-prompt state :runner "Place 1 [Credits] on Net Mercur")
        (is (= 1 (get-counters (refresh nm) :credit)) "1 credit placed on Net Mercur")
        (card-ability state :runner gr 0)
        (is (no-prompt? state :runner) "No Net Mercur prompt for 2nd stealth in run")
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :hq)
        (card-ability state :runner nm 0)
        (is (= "Net Mercur" (:title (:card (prompt-map :runner)))) "Net Mercur triggers itself"))))

(deftest net-mercur-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Net Mercur" "Cloak" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Net Mercur")
      (play-from-hand state :runner "Cloak")
      (play-from-hand state :runner "Refractor")
      (let [nm (get-resource state 0)
            cl (get-program state 0)
            refr (get-program state 1)]
        (is (changed? [(:credit (get-runner)) 0]
              (run-on state :hq)
              (card-ability state :runner refr 1)
              (click-card state :runner cl))
            "Used 1 credit from Cloak")
        (click-prompt state :runner "Place 1 [Credits] on Net Mercur")
        (is (= 1 (get-counters (refresh nm) :credit)) "1 credit placed on Net Mercur"))))

(deftest net-mercur-prevention-prompt-issue-4464
    ;; Prevention prompt. Issue #4464
    (do-game
      (new-game {:runner {:hand ["Feedback Filter" "Net Mercur"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (play-from-hand state :runner "Net Mercur")
      (let [nm (get-resource state 0)
            ff (get-hardware state 0)]
        (core/add-counter state :runner (refresh nm) :credit 4)
        (damage state :corp :net 2)
        (card-ability state :runner ff 0)
        (click-card state :runner nm)
        (click-card state :runner nm)
        (click-card state :runner nm)
        (card-ability state :runner ff 0)
        (click-prompt state :runner "Done")
        (is (= 1 (get-counters (refresh nm) :credit)) "Net Mercur has lost 3 credits"))))

(deftest network-exchange
  ;; ice install costs 1 more except for inner most
  (do-game
      (new-game {:corp {:deck [(qty "Paper Wall" 3)]}
                 :runner {:deck ["Network Exchange"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Network Exchange")
      (take-credits state :runner)
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 8 (:credit (get-corp))) "Paid 0 to install Paper Wall")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 6 (:credit (get-corp))) "Paid 1 extra  to install Paper Wall")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 3 (:credit (get-corp))) "Paid 1 extra  to install Paper Wall")))

(deftest network-exchange-architect-1st-sub-should-ignore-additional-install-cost
    ;; Architect 1st sub should ignore additional install cost
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]
                        :hand ["Architect"]}
                 :runner {:deck ["Network Exchange"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (take-credits state :corp) ; corp has 7 credits
      (play-from-hand state :runner "Network Exchange")
      (run-on state "HQ")
      (let [architect (get-ice state :hq 0)]
        (rez state :corp architect)
        (run-continue state)
        (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rez")
        (card-subroutine state :corp architect 0)
        (click-prompt state :corp "OK")
        (click-prompt state :corp "Ice Wall")
        (click-prompt state :corp "HQ")
        (is (= 3 (:credit (get-corp))) "Corp has 7 credits"))))

(deftest neutralize-all-threats
  ;; Neutralize All Threats - Access 2 cards from HQ, force trash first accessed card with a trash cost
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 2) "Breaker Bay Grid" "Elizabeth Mills"]}
                 :runner {:deck ["Neutralize All Threats"]}})
      (play-from-hand state :corp "Breaker Bay Grid" "New remote")
      (play-from-hand state :corp "Elizabeth Mills" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Neutralize All Threats")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action") ; access first Hedge Fund
      (click-prompt state :runner "No action") ; access second Hedge Fund
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (= 3 (:credit (get-runner))) "Forced to pay 2c to trash BBG")
      (is (= 1 (count (:discard (get-corp)))) "Breaker Bay Grid trashed")
      (run-empty-server state "Server 2")
      (is (not (no-prompt? state :runner)) "Runner prompt to trash Elizabeth Mills")))

(deftest neutralize-all-threats-interaction-with-trebuchet
    ;; Interaction with Trebuchet
    (do-game
      (new-game {:corp {:deck ["PAD Campaign" "Trebuchet"]}
                 :runner {:deck ["Neutralize All Threats"]}})
      (play-from-hand state :corp "Trebuchet" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Neutralize All Threats")
      (let [treb (get-ice state :hq 0)]
        (run-on state "HQ")
        (rez state :corp treb)
        (run-continue state)
        (card-subroutine state :corp treb 1)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (run-continue state)
        (run-continue state)
        (is (= 1 (count (:choices (prompt-map :runner)))) "Only one choice in prompt")
        (click-prompt state :runner "No action")
        (is (= 0 (count (:discard (get-corp)))) "PAD Campaign didn't get trashed"))))

(deftest new-angeles-city-hall
  ;; New Angeles City Hall - Avoid tags; trash when agenda is stolen
  (do-game
      (new-game {:corp {:deck ["SEA Source" "Breaking News"]}
                 :runner {:deck ["New Angeles City Hall"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "New Angeles City Hall")
      (let [nach (get-resource state 0)]
        (run-empty-server state "Archives")
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))))
        (play-from-hand state :corp "SEA Source")
        (click-prompt state :corp "0") ; default trace
        (click-prompt state :runner "0") ; Runner won't match
        (card-ability state :runner nach 0)
        (click-prompt state :runner "Done")
        (is (zero? (count-tags state)) "Avoided SEA Source tag")
        (is (= 4 (:credit (get-runner))) "Paid 2 credits")
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (is (= 1 (:agenda-point (get-runner))))
        (is (empty? (get-resource state)) "NACH trashed by agenda steal"))))

(deftest new-angeles-city-hall-trash-does-not-trigger-dummy-box
  ;; New Angeles City Hall trash doesn't trigger Dummy Box
  (do-game
      (new-game {:corp {:deck ["Breaking News"]}
                 :runner {:deck [(qty "New Angeles City Hall" 2) "Dummy Box"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "New Angeles City Hall")
      (play-from-hand state :runner "Dummy Box")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
    (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trash")))

(deftest new-angeles-city-hall-don-t-gain-siphon-credits-until-opportunity-to-avoid-tags-has-passed
    ;; don't gain Siphon credits until opportunity to avoid tags has passed
    (do-game
      (new-game {:runner {:deck ["Account Siphon" "New Angeles City Hall"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "New Angeles City Hall")
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :runner "Account Siphon")
      (let [nach (get-resource state 0)]
        (is (= 4 (:credit (get-runner))) "Have not gained Account Siphon credits until tag avoidance window closes")
        (card-ability state :runner nach 0)
        (card-ability state :runner nach 0)
        (click-prompt state :runner "Done")
        (is (zero? (count-tags state)) "Tags avoided")
        (is (= 10 (:credit (get-runner))) "10 credits siphoned")
        (is (= 3 (:credit (get-corp))) "Corp lost 5 credits"))))

(deftest no-free-lunch-gain-credits
  (do-game
    (new-game {:runner {:deck ["No Free Lunch"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "No Free Lunch")
    (is (changed? [(:credit (get-runner)) 3]
          (card-ability state :runner (get-resource state 0) 0))
        "gained 3 for lunch")
    (is (= 1 (count (:discard (get-runner)))) "Free lunch trashed")))

(deftest no-free-lunch-remove-tag
  (do-game
    (new-game {:runner {:hand ["No Free Lunch" "Rogue Trading"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Rogue Trading")
    (play-from-hand state :runner "No Free Lunch")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (count-tags state)) "+1 tag from Rogue Trading")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 0 (count-tags state)) "-1 tag from Free Lunch")
    (is (= 1 (count (:discard (get-runner)))) "Free lunch trashed")))

(deftest no-one-home
  ;; Prevent first tag or net damage of the turn if you beat trace0, then trash
  (do-game
      (new-game {:corp {:deck ["Data Mine" "SEA Source" "Scorched Earth"]}
                :runner {:deck [(qty "No One Home" 3) (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Data Mine" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "No One Home")
      (let [dm (get-ice state :archives 0)
            noh (get-resource state 0)]
        (run-on state "Archives")
        (rez state :corp dm)
        (run-continue state)
        (card-subroutine state :corp dm 0)
        (card-ability state :runner noh 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
        (run-continue state)
        (play-from-hand state :runner "No One Home")
        (take-credits state :runner)
        (play-from-hand state :corp "SEA Source")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (not (no-prompt? state :runner)) "Runner prompted to avoid tag")
        (card-ability state :runner (get-resource state 0) 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (click-prompt state :runner "Done")
        (is (= 3 (count (:discard (get-runner)))) "Two NOH trashed, 1 gamble played")
        (is (zero? (count-tags state)) "Tags avoided")
        (take-credits state :corp)
        (play-from-hand state :runner "No One Home")
        (take-credits state :runner)
        (gain-tags state :runner 1)
        (is (not (no-prompt? state :runner)) "Runner prompted to avoid tag")
        (click-prompt state :runner "Done")
        (core/gain state :corp :credit 4)
        (play-from-hand state :corp "Scorched Earth")
        (is (no-prompt? state :runner) "Runner not prompted to avoid meat damage"))
  ;; Ensure net damage prevention prompt still appears after receiving meat damage in a turn
  (testing "Damage order test"
    (do-game
      (new-game {:corp {:deck ["Data Mine"]}
                :runner {:deck [(qty "No One Home" 3) "Sure Gamble" "Respirocytes"]}})
      (play-from-hand state :corp "Data Mine" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "No One Home")
      (play-from-hand state :runner "Respirocytes")
      (let [dm (get-ice state :archives 0)
            noh (get-resource state 0)]
        (run-on state "Archives")
        (rez state :corp dm)
        (run-continue state)
        (card-subroutine state :corp dm 0)
        (card-ability state :runner noh 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (count (:hand (get-runner)))) "1 net damage prevented"))))))

(deftest off-campus-apartment-ability-shows-a-simultaneous-resolution-prompt-when-appropriate
    ;; ability shows a simultaneous resolution prompt when appropriate
    (do-game
      (new-game {:runner {:deck ["Street Peddler" "Off-Campus Apartment"
                                 "Underworld Contact" (qty "Spy Camera" 6)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Off-Campus Apartment" "Underworld Contact"])
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)]
        (card-ability state :runner oca 0)
        (click-card state :runner (find-card "Underworld Contact" (:hand (get-runner))))
        (is (= 2 (count (:hand (get-runner)))) "Drew a card from OCA")
        (card-ability state :runner oca 0)
        (click-card state :runner (find-card "Street Peddler" (:hand (get-runner))))
        ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
        (is (= 2 (count (prompt-buttons :runner))) "Simultaneous-resolution prompt is showing")
        (click-prompt state :runner "Off-Campus Apartment")
        (is (= 2 (count (:hand (get-runner)))) "Drew a card from OCA"))))

(deftest off-campus-apartment-second-ability-does-not-break-cards-that-are-hosting-others-e-g-street-peddler
    ;; second ability does not break cards that are hosting others, e.g., Street Peddler
    (do-game
      (new-game {:runner {:deck [(qty "Street Peddler" 2) "Off-Campus Apartment" (qty "Spy Camera" 6)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Street Peddler" "Off-Campus Apartment"])
      (core/move state :runner (find-card "Street Peddler" (:hand (get-runner))) :deck {:front true})
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)]
        (card-ability state :runner oca 0)
        (click-card state :runner (find-card "Street Peddler" (:hand (get-runner))))
        (click-prompt state :runner "Street Peddler")
        (let [ped1 (first (:hosted (refresh oca)))]
          (card-ability state :runner ped1 0)
          (click-prompt state :runner (last (prompt-buttons :runner))) ; choose Street Peddler
          (card-ability state :runner (refresh oca) 1)
          (click-card state :runner (get-resource state 1))
          (let [ped2 (first (:hosted (refresh oca)))]
            (card-ability state :runner ped2 0)
            (click-prompt state :runner (first (prompt-buttons :runner))) ; choose Spy Camera
            ;; the fact that we got this far means the bug is fixed
            (is (= 1 (count (get-hardware state))) "Spy Camera installed"))))))

(deftest off-campus-apartment-interaction-with-the-class-act-issue-4282
    ;; Interaction with The Class Act. Issue #4282
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Off-Campus Apartment" "The Class Act"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Off-Campus Apartment")
      (let [oca (get-resource state 0)]
        (card-ability state :runner oca 0)
        (click-card state :runner "The Class Act")
        (click-card state :runner (last (:set-aside (get-runner))))
        (is (= 1 (count (:hand (get-runner)))))
        (take-credits state :runner)
        (is (= 5 (count (:hand (get-runner)))) "Draw 4 cards from The Class Act")
        (take-credits state :corp)
        (is (= 5 (count (:hand (get-runner)))) "Don't draw any more"))))

(deftest officer-frank
  ;; Officer Frank - meat damage to trash 2 from HQ
  (do-game
    (new-game {:corp {:deck ["Swordsman" (qty "Hedge Fund" 2)]}
               :runner {:deck ["Officer Frank" "Skulljack" (qty "Respirocytes" 4)]}})
    (play-from-hand state :corp "Swordsman" "Archives")
    (take-credits state :corp)
    (starting-hand state :runner ["Officer Frank" "Skulljack" "Respirocytes" "Respirocytes" "Respirocytes" "Respirocytes"])
    (play-from-hand state :runner "Officer Frank")
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
    (play-from-hand state :runner "Skulljack")
    (is (= 3 (count (:hand (get-runner)))) "Took 1 core damage")
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
    (let [sm (get-ice state :archives 0)]
      (run-on state :archives)
      (rez state :corp sm)
      (run-continue state)
      (card-subroutine state :corp sm 1)
      (run-continue state :movement)
      (run-jack-out state))
    (is (= 2 (count (:hand (get-runner)))) "Took 1 net damage")
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
    (play-from-hand state :runner "Respirocytes")
    (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 2 (count (:discard (get-corp)))) "Two cards trashed from HQ")))

(deftest order-of-sol-get-down-to-zero-credits-from-playing-oos
    ;; Get down to zero credits from playing OoS
    (do-game
      (new-game {:runner {:hand ["Order of Sol"]
                          :credits 2}})
      (take-credits state :corp)
      (is (changed? [(:credit (get-runner)) -1]
            (play-from-hand state :runner "Order of Sol"))
          "Only spends 1 credit total to install Order of Sol")
      (is (last-log-contains? state "Runner uses Order of Sol to gain 1 [Credits]."))))

(deftest order-of-sol-get-down-to-zero-credits-from-playing-an-event
    ;; Get down to zero credits from playing an event
    (do-game
      (new-game {:runner {:hand ["Order of Sol" "Sure Gamble"]
                          :credits 7}})
      (take-credits state :corp)
      (play-from-hand state :runner "Order of Sol")
      (is (changed? [(:credit (get-runner)) 5]
            (play-from-hand state :runner "Sure Gamble"))
          "Gain 5 total credits from playing Sure Gamble")
      (is (last-n-log-contains? state 2 "Runner uses Order of Sol to gain 1 [Credits]."))))

(deftest order-of-sol-losing-credits
    ;; Losing credits
    (do-game
      (new-game {:corp {:id "Spark Agency: Worldswide Reach"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["PAD Campaign"]}
                 :runner {:hand ["Order of Sol"]
                          :credits 3}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Order of Sol")
      (is (changed? [(:credit (get-runner)) 0]
            (rez state :corp (get-content state :remote1 0)))
          "Gain and lose 1 credit")
      (is (last-log-contains? state "Runner uses Order of Sol to gain 1 [Credits]."))))

(deftest pad-tap
  ;; PAD Tap
  (do-game
      (new-game {:corp {:deck ["Melange Mining Corp."]}
                 :runner {:deck ["PAD Tap"]}})
      (play-from-hand state :corp "Melange Mining Corp." "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "PAD Tap")
      (let [mel (get-content state :remote1 0)
            tap (get-resource state 0)]
        (take-credits state :runner)
        (let [credits (:credit (get-runner))]
          (click-credit state :corp)
          (is (zero? (count (prompt-buttons :runner))) "Runner should have no prompts from PAD Tap")
          (is (= credits (:credit (get-runner))) "Runner shouldn't gain PAD Tap credits from clicking for a credit"))
        (let [credits (:credit (get-runner))]
          (rez state :corp mel)
          (core/gain state :corp :click 10)
          (card-ability state :corp mel 0)
          (is (= (inc credits) (:credit (get-runner))) "Runner should gain 1 credit from PAD Tap triggering from Melange Mining Corp. ability")
          (card-ability state :corp mel 0) ;; Triggering Melange a second time
          (is (zero? (count (prompt-buttons :runner))) "Runner should have no prompts from PAD Tap"))
        (take-credits state :corp)
        (take-credits state :runner)
        (is (zero? (-> (get-runner) :discard count)) "Runner should have 0 cards in Heap")
        (let [credits (:credit (get-corp))
              clicks (:click (get-corp))]
          (card-side-ability state :corp tap 0)
          (is (= (- credits 3) (:credit (get-corp))) "PAD Tap ability should cost Corp 3 credits")
          (is (= (dec clicks) (:click (get-corp))) "PAD Tap ability should cost Corp 1 click")))))

(deftest pad-tap-only-pays-out-once-per-turn-issue-4593
    ;; Only pays out once per turn. Issue #4593
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Rashida Jaheem"]}
                 :runner {:hand ["PAD Tap"]}})
      (play-from-hand state :corp "Rashida Jaheem" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "PAD Tap")
      ;; Manually ending the turn to trigger Rashida early
      (dotimes [_ (get-in @state [:runner :click])]
        (click-credit state :runner))
      (end-turn state :runner)
      (let [credits (:credit (get-runner))]
        (rez state :corp (get-content state :remote1 0))
        (card-ability state :corp (get-content state :remote1 0) 0)
        (is (no-prompt? state :corp) "No optional prompt as we've not started the turn yet")
        (start-turn state :corp)
        (card-ability state :corp (get-content state :remote1 0) 0)
        (click-prompt state :corp "Yes")
        (is (= (inc credits) (:credit (get-runner))) "Runner should gain 1 from PAD Tap"))
      (let [credits (:credit (get-runner))]
        (play-from-hand state :corp "Hedge Fund")
        (is (= credits (:credit (get-runner))) "Runner should gain 1 from PAD Tap"))))

(deftest paige-piper-interaction-with-frantic-coding-issue-2190
    ;; interaction with Frantic Coding. Issue #2190
    (do-game
      (new-game {:runner {:hand ["Paige Piper" "Frantic Coding" "Frantic Coding"]
                          :deck [(qty "Sure Gamble" 3) (qty "Gordian Blade" 2)
                                 "Ninja" (qty "Bank Job" 3) (qty "Indexing" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Paige Piper")
      (click-prompt state :runner "No")
      (take-credits state :runner) ; now 8 credits
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (click-prompt state :runner "Gordian Blade")
      (is (= 1 (count (get-program state))) "Installed Gordian Blade")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "0")
      (is (= 1 (count (:discard (get-runner)))) "Paige Piper intervention stopped Frantic Coding from trashing 9 cards")
      (is (= 5 (:credit (get-runner))) "No charge to install Gordian")
      ;; a second Frantic Coding will not trigger Paige (once per turn)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (click-prompt state :runner "Ninja")
      (is (= 2 (count (get-program state))) "Installed Ninja")
      (is (= 11 (count (:discard (get-runner)))) "11 cards in heap")
      (is (= 2 (:credit (get-runner))) "No charge to install Ninja")))

(deftest paige-piper-interaction-with-rolodex-issue-2694
    ;; interaction with Rolodex. Issue #2694
    (do-game
      (new-game {:runner {:deck [(qty "Gordian Blade" 2)
                                 (qty "Bank Job" 3)
                                 (qty "Rolodex" 2)]
                          :hand ["Paige Piper" "Rolodex"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Paige Piper")
      (click-prompt state :runner "No")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Rolodex")
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :runner)))
          "Runner has simultaneous resolution prompt")
      (is (= #{"Rolodex" "Paige Piper"} (set (prompt-titles :runner)))
          "Both Rolodex and Paige Piper can be chosen")
      (click-prompt state :runner "Paige Piper")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "2")
      (is (= ["Rolodex" "Rolodex"] (map :title (:discard (get-runner)))) "Both Rolodex are now in the discard")
      (click-prompt state :runner "Gordian Blade")
      (click-prompt state :runner "Bank Job")
      (click-prompt state :runner "Bank Job")
      (click-prompt state :runner "Gordian Blade")
      (click-prompt state :runner "Bank Job")
      (click-prompt state :runner "Done")
      (is (no-prompt? state :corp) "Corp has no wait prompts")
      (is (no-prompt? state :runner) "Runner has no ability prompts")))

(deftest paladin-poemu
  ;; Paladin Poemu
  (do-game
      (new-game {:corp {:deck ["Project Vitruvius"]}
                 :runner {:hand ["Corroder" "Fan Site" "Dummy Box" "Sacrificial Construct" "Misdirection" "Hernando Cortez" "Paladin Poemu"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Paladin Poemu")
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Dummy Box")
      (play-from-hand state :runner "Sacrificial Construct")
      (play-from-hand state :runner "Misdirection")
      (let [pp (get-resource state 0)
            fs (get-resource state 1)
            sac (get-resource state 3)
            misd (get-program state 0)]
        (is (= 0 (get-counters (refresh pp) :credit)) "No credits on Poemu yet")
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh pp) :credit)) "+1c from start of turn")
        (run-empty-server state "HQ")
        (click-prompt state :runner "Steal")
        (is (= 2 (get-counters (refresh pp) :credit)) "+1c from steal")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh pp) :credit)) "+1c from start of turn")
        (take-credits state :runner)
        (click-card state :runner fs)
        (is (no-prompt? state :runner) "No prompt to prevent trashing with Dummy Box")
        (take-credits state :corp)
        (take-credits state :runner)
        (click-card state :runner (refresh misd))
        (is (not (no-prompt? state :runner)) "Prompt to prevent trashing with Sacrificial Construct")
        (card-ability state :runner sac 0)
        (click-prompt state :runner "Done")
        (take-credits state :corp)
        (is (changed? [(:credit (get-runner)) 0]
              (play-from-hand state :runner "Corroder")
              (click-card state :runner pp)
              (click-card state :runner pp))
            "Used Poemu to install Corroder for free")
        (play-from-hand state :runner "Hernando Cortez")
        (is (no-prompt? state :runner) "No pay-credits prompt on the install of a Connection"))))

(deftest paladin-poemu-async-issues-are-handled-properly-issue-4784
    ;; Async issues are handled properly. Issue #4784
    (do-game
      (new-game {:corp {:deck ["Project Vitruvius"]}
                 :runner {:hand ["Paladin Poemu"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Paladin Poemu")
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (click-card state :runner "Paladin Poemu")
      (is (find-card "Paladin Poemu" (:discard (get-runner))) "Paladin Poemu should be trashed")
      (is (no-prompt? state :runner) "Runner has no prompt")
      (is (no-prompt? state :corp) "Corp has no prompt")))

(deftest paladin-poemu-bin-breakers-trigger-paladin-poemu
    ;; Bin breakers trigger Paladin Poemu
    (do-game
      (new-game {:corp {:hand ["Lotus Field"]}
                 :runner {:hand ["Paladin Poemu"]
                          :discard ["Black Orchestra"]}})
      (play-from-hand state :corp "Lotus Field" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Paladin Poemu")
      (let [pp (get-resource state 0)]
        (dotimes [_ 3]
          (take-credits state :runner)
          (take-credits state :corp))
        (is (= 3 (get-counters (refresh pp) :credit)) "Paladin has 3c")
        (is (changed? [(:credit (get-runner)) 0]
              (run-on state "HQ")
              (run-continue state)
              (click-prompt state :runner "Yes")
              ; install BO
              (dotimes [_ 3]
            (click-card state :runner pp))
              (is (zero? (count (:discard (get-runner)))) "BO installed from heap")
              (is (= 1 (count (get-program state))) "BO installed")
              (is (zero? (get-counters (refresh pp) :credit)) "Paladin spent 3c"))
            "Used Poemu to install Black Orchestra for free"))))

(deftest patron
  ;; Patron
  (do-game
      (new-game {:corp {:deck ["Jackson Howard"]}
                 :runner {:deck [(qty "Patron" 4) (qty "Easy Mark" 4)]}})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Patron")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (click-prompt state :runner "Server 1")
      (is (= 4 (count (:hand (get-runner)))) "Starts with 4 cards")
      (run-empty-server state "Server 1")
      (is (= 6 (count (:hand (get-runner)))) "Drew 2 cards")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 6 (count (:hand (get-runner)))) "Drew no cards")
      (play-from-hand state :runner "Easy Mark")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Server 1")
      (run-empty-server state "Archives")
      (is (= 5 (count (:hand (get-runner)))) "Did not draw cards when running other server")))

(deftest patron-manually-selecting-during-step-1-2-does-not-show-a-second-prompt-at-start-of-turn-issue-1744
    ;; Manually selecting during Step 1.2 does not show a second prompt at start of turn. Issue #1744.
    (do-game
      (new-game {:runner {:deck [(qty "Patron" 3) (qty "Jak Sinclair" 3)]
                          :hand ["Patron" "Jak Sinclair"]
                          :credits 15}})
      (take-credits state :corp)
      (play-from-hand state :runner "Patron")
      (play-from-hand state :runner "Jak Sinclair")
      (take-credits state :runner)
      (let [p (get-resource state 0)
            j (get-resource state 1)]
        (take-credits state :corp)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (card-ability state :runner p 0)
        (click-prompt state :runner "Archives")
        (card-ability state :runner j 0)
        (click-prompt state :runner "Archives")
        (run-continue state)
        (end-phase-12 state :runner)
        (is (no-prompt? state :runner) "No second prompt for Patron - used already"))))

(deftest paule-s-cafe
  (do-game
      (new-game {:runner {:hand ["Paule's Café" "Hernando Cortez" "Kati Jones" "Magnum Opus" "Desperado" "Fan Site" "Corroder"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10 :click 10)
      (play-from-hand state :runner "Paule's Café")
      (play-from-hand state :runner "Hernando Cortez")
      (play-from-hand state :runner "Kati Jones")
      (play-from-hand state :runner "Fan Site")
      (let [pau (get-resource state 0)]
        (card-ability state :runner pau 0)
        (click-card state :runner (find-card "Magnum Opus" (:hand (get-runner))))
        (card-ability state :runner pau 0)
        (click-card state :runner (find-card "Desperado" (:hand (get-runner))))
        (card-ability state :runner pau 0)
        (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
        (is (= 0 (count (:hand (get-runner)))) "Hosted Mopus, Desperado and Corroder on the Café")
        (let [mo (find-card "Magnum Opus" (:hosted (refresh pau)))
              des (find-card "Desperado" (:hosted (refresh pau)))
              cor (find-card "Corroder" (:hosted (refresh pau)))]
          (card-ability state :runner pau 1)
          (is (changed? [(:credit (get-runner)) -4]
                (click-card state :runner mo))
              "Pay 4 for MOpus install (1+5-2)")
          (is (second-last-log-contains? state "Runner pays 1 [Credits] to use Paule's Café to install hosted card\\.") "Correct message for Paule usage")
          (is (last-log-contains? state "Runner pays 3 [Credits] to install Magnum Opus using Paule's Café\\.") "Correct message for MOpus install")
          (card-ability state :runner pau 1)
          (is (changed? [(:credit (get-runner)) -4]
                (click-card state :runner des))
              "Pay 4 for Desperado install (1+3)")
          (is (second-last-log-contains? state "Runner pays 1 [Credits] to use Paule's Café to install hosted card\\.") "Correct message for Paule usage")
          (is (last-log-contains? state "Runner pays 3 [Credits] to install Desperado using Paule's Café\\.") "Correct message for Desperado install")
          (take-credits state :runner)
          (card-ability state :runner pau 1)
          (is (changed? [(:credit (get-runner)) -3]
                (click-card state :runner cor))
              "Pay 3 for Corroder install in Corp turn (1+2)")
          (is (second-last-log-contains? state "Runner pays 1 [Credits] to use Paule's Café to install hosted card\\.") "Correct message for Paule usage")
          (is (last-log-contains? state "Runner pays 2 [Credits] to install Corroder using Paule's Café\\.") "Correct message for Corroder install")))))

(deftest paule-s-cafe-can-t-lower-cost-below-1-issue-4816
    ;; Can't lower cost below 1. Issue #4816
    (do-game
      (new-game {:runner {:hand ["Paule's Café" "Hernando Cortez" "Kati Jones""Fan Site" "Miss Bones" "Corroder"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10 :click 10)
      (play-from-hand state :runner "Paule's Café")
      (play-from-hand state :runner "Hernando Cortez")
      (play-from-hand state :runner "Kati Jones")
      (play-from-hand state :runner "Fan Site")
      (play-from-hand state :runner "Miss Bones")
      (let [pau (get-resource state 0)]
        (card-ability state :runner pau 0)
        (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
        (is (= 0 (count (:hand (get-runner)))) "Hosted Corroder on the Café")
        (let [cor (find-card "Corroder" (:hosted (refresh pau)))]
          (is (changed? [(:credit (get-runner)) -1]
                (card-ability state :runner pau 1)
                (click-card state :runner cor))
              "Pay 1 credit for Corroder (2 - 4 + 1 base)")
          (is (second-last-log-contains? state "Runner pays 1 [Credits] to use Paule's Café to install hosted card\\.") "Correct message for Paule usage")
          (is (last-log-contains? state "Runner pays 0 [Credits] to install Corroder using Paule's Café\\.") "Correct message for Corroder install")))))

(deftest penumbral-toolkit-install-cost-reduction-after-hq-run
    ;; install cost reduction after HQ run
    (do-game
      (new-game {:runner {:deck [(qty "Penumbral Toolkit" 3)]}})
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (is (changed? [(:credit (get-runner)) -2]
            (play-from-hand state :runner "Penumbral Toolkit"))
          "No cost reduction without run")
      (run-empty-server state :rd)
      (is (changed? [(:credit (get-runner)) -2]
            (play-from-hand state :runner "Penumbral Toolkit"))
          "No cost reduction after run on R&D")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (changed? [(:credit (get-runner)) 0]
            (play-from-hand state :runner "Penumbral Toolkit"))
          "Cost reduction after run on HQ")
      (is (= 3 (count (get-resource state))) "Installed all three cards")))

(deftest penumbral-toolkit-install-cost-reduction-applies-during-success-phase
  ;; install cost reduction applies during success phase
  (do-game
   (new-game {:corp {:hand ["Hostile Takeover"]}
              :runner {:hand ["Penumbral Toolkit" "Pantograph"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Pantograph")
   (run-empty-server state :hq)
   (is (:successful (get-run)) "The Run is in the success phase")
   (click-prompt state :runner "Steal")
   (is (changed? [(:credit (get-runner)) 0]
         (click-card state :runner "Penumbral Toolkit"))
       "Cost reduction after run on HQ")
   (is (= 1 (count (get-resource state))) "Installed Penumbral Toolkit")))

(deftest penumbral-toolkit-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Penumbral Toolkit" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Penumbral Toolkit")
      (play-from-hand state :runner "Refractor")
      (run-on state :hq)
      (let [pt (get-resource state 0)
            refr (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 2]
              (dotimes [_ 2]
                             (card-ability state :runner (refresh pt) 0)))
            "Took 2 credits off of Penumbral Toolkit the traditional way.")
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner refr 1)
              (click-card state :runner (refresh pt))
              (card-ability state :runner (refresh refr) 1)
              (click-card state :runner (refresh pt)))
            "Used 2 credits from Penumbral Toolkit")
        (is (not-empty (:discard (get-runner))) "Empty Ghost Runner trashed"))))

(deftest personal-workshop-removes-token-on-start-of-turn-and-installs
    ;; Removes token on start of turn and installs
    (do-game
     (new-game {:runner {:deck ["Personal Workshop" "Medium"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Personal Workshop")
     (let [pw (get-resource state 0)]
       ;; host medium and remove 2 of 3 counters
       (card-ability state :runner pw 0)
       (click-card state :runner (find-card "Medium" (:hand (get-runner))))
       (card-ability state :runner pw 2)
       (click-card state :runner (first (:hosted (refresh pw))))
       (click-prompt state :runner "2")
       ;; come back around to runner start of turn, remove last token
       (take-credits state :runner)
       (take-credits state :corp)
       (click-card state :runner (first (:hosted (refresh pw))))
       (is (= 1 (count (get-program state))))
       (is (= "Medium" (:title (get-program state 0)))))))

(deftest personal-workshop-manually-spending-credits-removes-tokens-and-installs
    ;; Manually spending credits removes tokens and installs
    (do-game
     (new-game {:runner {:deck ["Personal Workshop" "Medium"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Personal Workshop")
     (let [pw (get-resource state 0)]
       ;; host medium and remove all 3 counters
       (card-ability state :runner pw 0)
       (click-card state :runner (find-card "Medium" (:hand (get-runner))))
       (card-ability state :runner pw 2)
       (click-card state :runner (first (:hosted (refresh pw))))
       (click-prompt state :runner "3")
       (is (= 1 (count (get-program state))))
       (is (= "Medium" (:title (get-program state 0)))))))

(deftest personal-workshop-can-host-and-install-more-than-once-per-turn
    ;; Can host and install more than once per turn
    (do-game
     (new-game {:runner {:deck ["Personal Workshop" "Medium" "Corroder"]}})
     (take-credits state :corp)
     (core/gain state :runner :credit 5)
     (play-from-hand state :runner "Personal Workshop")
     (let [pw (get-resource state 0)]
       ;; host medium and remove all 3 counters
       (card-ability state :runner pw 0)
       (click-card state :runner (find-card "Medium" (:hand (get-runner))))
       (card-ability state :runner pw 2)
       (click-card state :runner (first (:hosted (refresh pw))))
       (click-prompt state :runner "3")
       (is (= 1 (count (get-program state))))
       (is (= "Medium" (:title (get-program state 0))))
       ;; do the same with corroder
       (card-ability state :runner pw 0)
       (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
       (card-ability state :runner pw 2)
       (click-card state :runner (first (:hosted (refresh pw))))
       (click-prompt state :runner "2")
       (is (= 2 (count (get-program state))))
       (is (= "Corroder" (:title (get-program state 1)))))))

(deftest power-tap
  ;; Power Tap
  (do-game
    (new-game {:corp {:deck ["Restructured Datapool"]}
               :runner {:deck ["Power Tap"]}})
    (play-and-score state "Restructured Datapool")
    (let [agenda (get-scored state :corp 0)
          tags (count-tags state)
          credits (:credit (get-runner))]
      (card-ability state :corp agenda 0)
      (is (= credits (:credit (get-runner))) "Runner shouldn't gain any credits from trace")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= (inc tags) (count-tags state)) "Runner should gain 1 tag from losing trace"))
    (take-credits state :corp)
    (play-from-hand state :runner "Power Tap")
    (take-credits state :runner)
    (let [agenda (get-scored state :corp 0)
          tags (count-tags state)
          credits (:credit (get-runner))]
      (card-ability state :corp agenda 0)
      (is (= (inc credits) (:credit (get-runner))) "Runner should gain 1 credit from trace initiation")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= (inc tags) (count-tags state)) "Runner should gain 1 tag from losing trace"))))

(deftest professional-contacts
  ;; Professional Contacts - Click to gain 1 credit and draw 1 card
  (do-game
    (new-game {:runner {:deck [(qty "Professional Contacts" 3)
                               (qty "Sure Gamble" 2)
                               (qty "Shiv" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Professional Contacts")
    (let [proco (get-resource state 0)]
      (card-ability state :runner proco 0)
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (:credit (get-runner))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-runner)))) "Drew 1 card")
      (card-ability state :runner proco 0)
      (is (= 1 (:click (get-runner))) "Spent 1 click")
      (is (= 2 (:credit (get-runner))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-runner)))) "Drew 1 card"))))

(deftest psych-mike
  ;; Psych Mike
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Psych Mike" "Deep Data Mining"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Psych Mike")
      (let [credits (:credit (get-runner))]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Yes")
        (is (= (inc credits) (:credit (get-runner))) "Psych Mike should give 1 credit for accessing 1 card"))
      (let [credits (:credit (get-runner))]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= credits (:credit (get-runner))) "Psych Mike should give 0 credits for second run of the turn"))
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Deep Data Mining")
      (let [credits (:credit (get-runner))]
        (run-continue state)
        (dotimes [_ 5]
          (click-prompt state :runner "No action"))
        (click-prompt state :runner "Yes")
        (is (= (+ credits 5) (:credit (get-runner))) "Psych Mike should give 5 credits for DDM accesses"))
        (run-empty-server state "HQ")
        (click-prompt state :runner "No action")
    (is (not (last-log-contains? state "Psych Mike to gain 0")) "No log should be printed")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [credits (:credit (get-runner))]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No")
        (is (= credits (:credit (get-runner))) "Psych Mike should give 0 credits when declining to use her ability"))
      (testing "Regression test for #3828"
        (take-credits state :runner)
        (take-credits state :corp)
        (let [credits (:credit (get-runner))]
          (run-empty-server state "HQ")
          (click-prompt state :runner "No action")
          (is (= credits (:credit (get-runner))) "Psych Mike should give 0 credits for accessing 1 card from HQ"))
        (let [credits (:credit (get-runner))]
          (run-empty-server state "R&D")
          (click-prompt state :runner "No action")
          (click-prompt state :runner "Yes")
          (is (= (inc credits) (:credit (get-runner)))
              "Psych Mike should give 1 credit for second run of the turn, if first on HQ")))))

(deftest psych-mike-vs-upgrades
    ;; vs upgrades
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 5)]
                        :hand ["Bryan Stinson"]}
                 :runner {:deck ["Psych Mike"]}})
      (play-from-hand state :corp "Bryan Stinson" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Psych Mike")
      (let [credits (:credit (get-runner))]
        (run-empty-server state "R&D")
        (click-prompt state :runner "Unrezzed upgrade")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Yes")
        (is (= (inc credits) (:credit (get-runner))) "Psych Mike should give 1 credit for accessing 1 card"))))

(deftest reclaim-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:runner {:deck ["Reclaim" "Mimic" "Clone Chip"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Mimic" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Reclaim")
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (click-card state :runner (find-card "Clone Chip" (:hand (get-runner))))
      (click-prompt state :runner (find-card "Mimic" (:discard (get-runner))))
      (is (= 1 (count (get-program state))) "1 Program installed")
      (is (= 2 (:credit (get-runner))) "Runner paid install cost")
      (is (last-n-log-contains? state 2 "Clone Chip"))
      (is (second-last-log-contains? state "uses Reclaim to install Mimic"))))

(deftest reclaim-no-cards-in-hand
    ;; No cards in hand
    (do-game
      (new-game {:runner {:deck ["Reclaim"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "No Reclaim prompt")))

(deftest reclaim-cannot-use-if-no-valid-targets-in-heap-already
    ;; Cannot use if no valid targets in Heap already
    (do-game
      (new-game {:runner {:deck ["Reclaim" "Mimic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Reclaim")
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "No Reclaim prompt")))

(deftest reclaim-can-t-afford-to-install-card
    ;; Can't afford to install card
    (do-game
      (new-game {:runner {:deck ["Reclaim" "Alpha" "Sure Gamble"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Alpha" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Reclaim")
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "No Reclaim prompt")))

(deftest reclaim-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:deck ["Blacklist"]}
                 :runner {:deck ["Reclaim" "Mimic" "Clone Chip"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (core/move state :runner (find-card "Mimic" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Reclaim")
      (is (empty? (get-program state)) "No programs installed")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5c.")
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "Reclaim prompt did not come up")))

(deftest red-team
  ;; Red Team
  (do-game
      (new-game {:runner {:deck ["Red Team"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Red Team")
      (is (= 0 (:credit (get-runner))) "Runner has 0 credits")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "Cancel")
      (is (no-prompt? state :runner) "Red Team prompt is gone")
      (is (= 3 (:click (get-runner))) "Runner still has 3 clicks left")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "HQ")
      (run-continue state)
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (is (= 9 (get-counters (get-resource state 0) :credit)) "Red team has 9 credits remaining")
      (click-prompt state :runner "No action")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= ["Archives" "R&D" "Cancel"] (prompt-buttons :runner)) "HQ no longer shown")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (is (= 6 (:credit (get-runner))) "Runner has 6 credits")
      (is (= 6 (get-counters (get-resource state 0) :credit)) "Red team has 6 credits remaining")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= ["Archives" "Cancel"] (prompt-buttons :runner)) "HQ and R&D no longer shown")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (is (= 9 (:credit (get-runner))) "Runner has 9 credits")
      (is (= 3 (get-counters (get-resource state 0) :credit)) "Red team has 3 credits remaining")
      (core/gain state :runner :click 1)
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-runner))) "Runner still has 9 credits")
      (is (= 3 (get-counters (get-resource state 0) :credit)) "Red team still has 3 credits remaining")))

(deftest red-team-is-interactive
  ;; Red Team ability is interactive
  (do-game
    (new-game {:runner {:hand ["Red Team" "World Tree"]
                        :deck ["Liberated Account"]
                        :credits 11}})
    (take-credits state :corp)
    (play-from-hand state :runner "Red Team")
    (play-from-hand state :runner "World Tree")
    (card-ability state :runner (get-resource state 0) 0)
    (click-prompt state :runner "Archives")
    (run-continue state)
    (click-prompt state :runner "Red Team")
    (click-card state :runner "Red Team")
    (click-prompt state :runner "Liberated Account")))

(deftest rolodex
  ;; Rolodex - Full test
  (do-game
    (new-game {:runner {:deck ["Rolodex" "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron"]}})
    (starting-hand state :runner ["Rolodex"])
    (is (= 1 (count (:hand (get-runner)))))
    (take-credits state :corp)
    (play-from-hand state :runner "Rolodex")
    (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
    ;; try starting over
    (click-prompt state :runner "Start over")
    (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
    (click-prompt state :runner "Done")
    (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
    (is (= "Desperado" (:title (second (:deck (get-runner))))))
    (is (= "Diesel" (:title (second (rest (:deck (get-runner)))))))
    (is (= "Corroder" (:title (second (rest (rest (:deck (get-runner))))))))
    (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-runner)))))))))
    (trash state :runner (get-resource state 0))
    (is (last-log-contains? state "Sure Gamble, Desperado, and Diesel")
        "Rolodex did log trashed card names")
    (is (= 4 (count (:discard (get-runner)))) "Rolodex mills 3 cards when trashed")
    (is (= "Corroder" (:title (first (:deck (get-runner))))))))

(deftest rosetta-2-0
  ;; Rosetta 2.0 remove an installed program from the game and install one from the heap lower install cost
  (do-game
    (new-game {:runner {:deck ["Gordian Blade"]
                        :hand ["Rosetta 2.0" "Corroder"]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Rosetta 2.0")
    (play-from-hand state :runner "Corroder")
    (let [rosetta (get-resource state 0)
          corroder (get-program state 0)]
      (is (= 3 (core/available-mu state)) "Corrder cost 1 mu")
      (is (= 5 (:credit (get-runner))) "Starting with 5 credits")
      (card-ability state :runner rosetta 0)
      (click-card state :runner corroder)
      (click-prompt state :runner (find-card "Gordian Blade" (:deck (get-runner))))
      (is (= 3 (core/available-mu state)) "Gordian cost 1 mu, Corroder freed")
      (is (= 3 (:credit (get-runner))) "Ending with 3 credits")
      (is (= 1 (count (:rfg (get-runner)))) "Corroder removed from game")
      (is (= 1 (count (get-program state))) "One program installed")
      (is (= "Gordian Blade" (:title (get-program state 0))) "Gordian installed"))))

(deftest sacrificial-construct
  ;; Sacrificial Construct - Trash to prevent trash of installed program or hardware
  (do-game
    (new-game {:runner {:deck [(qty "Sacrificial Construct" 2) "Cache"
                               "Motivation" "Astrolabe"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "Sacrificial Construct")
    (play-from-hand state :runner "Sacrificial Construct")
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Motivation")
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner)
    (trash state :runner (get-resource state 2))
    (is (no-prompt? state :runner) "Sac Con not prompting to prevent resource trash")
    (trash state :runner (get-program state 0))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 2 (count (:discard (get-runner)))) "Sac Con trashed")
    (is (= 1 (count (get-program state))) "Cache still installed")
    (trash state :runner (get-hardware state 0))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 3 (count (:discard (get-runner)))) "Sac Con trashed")
    (is (= 1 (count (get-hardware state))) "Astrolabe still installed")))

(deftest safety-first
  ;; Safety First - Reduce hand size by 2, draw 1 at turn end if below maximum
  (do-game
    (new-game {:runner {:deck [(qty "Safety First" 3) (qty "Cache" 3)]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Safety First" "Safety First" "Cache"])
    (play-from-hand state :runner "Safety First")
    (is (= 3 (hand-size :runner)) "Max hand size reduced by 2")
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Drew 1 card at end of turn")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (count (:hand (get-runner)))) "Drew no cards, at maximum")))

(deftest salsette-slums
  ;; Salsette Slums - Once per turn, when the trash cost of a card is paid, optionally remove from the game
  (do-game
    (new-game {:corp {:hand ["Hostile Infrastructure" "Tech Startup" "Thomas Haas"]
                      :deck [(qty "Hedge Fund" 3)]}
               :runner {:deck [(qty "Salsette Slums" 2) (qty "Sure Gamble" 3)]}})
    ;; Use Hostile Infrastructure to ensure on-trash effects don't fire.
    (play-from-hand state :corp "Tech Startup" "New remote")
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (play-from-hand state :corp "Thomas Haas" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Salsette Slums")
    (play-from-hand state :runner "Salsette Slums")
    (core/gain state :runner :credit 2)
    (core/gain state :runner :click 4)
    (let [hostile (get-content state :remote2 0)]
      (is (= 3 (count (:hand (get-runner)))) "Runner started this part with three cards in hand")
      (rez state :corp hostile)
      (run-empty-server state "Server 1")
      (is (not (no-prompt? state :runner)) "Prompting to trash.")
      (click-prompt state :runner "[Salsette Slums] Remove card from game")
      (is (no-prompt? state :runner) "All prompts done")
      (is (= 3 (count (:hand (get-runner)))) "On-trash ability of other Hostile didn't fire")
      (is (= "Tech Startup" (:title (last (:rfg (get-corp))))) "Tech Startup was removed from game")
      (is (= 2 (:credit (get-runner))) "Runner paid the trash cost.")
      (is (not (:run @state)) "Run is over")
      (run-empty-server state :remote2)
      (is (not (no-prompt? state :runner)) "Prompting to trash")
      (is (= ["No action"] (prompt-buttons :runner)) "Can't prompt to trash as can't afford trash cost")
      (is (:run @state) "Run is still occurring")
      (click-prompt state :runner "No action")
      (run-empty-server state :remote3)
      (is (not (no-prompt? state :runner)) "Prompting to trash")
      (is (= ["[Salsette Slums] Remove card from game" "Pay 1 [Credits] to trash" "No action"]
             (prompt-titles :runner))
          "Second Salsette Slums can be used")
      (click-prompt state :runner "[Salsette Slums] Remove card from game")
      (is (= 2 (count (:rfg (get-corp)))) "Two cards should be RFG now"))))

(deftest same-old-thing
  ;; Same Old Thing
  (do-game
      (new-game {:runner {:hand [(qty "Same Old Thing" 2)]
                          :discard ["Lucky Find" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Same Old Thing")
      (is (changed? [(:credit (get-runner)) 4]
            (card-ability state :runner (get-resource state 0) 0)
            (click-card state :runner "Sure Gamble"))
          "Sure Gamble is played from the Heap")
      (is (= 1 (:click (get-runner))) "One click left")
      (play-from-hand state :runner "Same Old Thing")
      (core/gain state :runner :click 3)
      (is (changed? [(:credit (get-runner)) 6]
            (card-ability state :runner (get-resource state 0) 0)
            (click-card state :runner "Lucky Find"))
          "Lucky Find is played from the Heap")
      (is (zero? (:click (get-runner))) "No clicks left")))

(deftest scrubber
  ;; Scrubber
  (do-game
      (new-game {:corp {:deck ["The Board"]}
                 :runner {:deck ["Scrubber"]}})
      (play-from-hand state :corp "The Board" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 1 (count (prompt-buttons :runner))) "Runner doesn't have enough credits to trash")
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Scrubber")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))) "Runner should only have 5 credits in pool")
      (run-empty-server state "Server 1")
      (is (= 2 (count (prompt-buttons :runner))) "Runner can use Scrubber credits to trash")
      (click-prompt state :runner "Pay 7 [Credits] to trash")
      (click-card state :runner "Scrubber")
      (click-card state :runner "Scrubber")
      (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board and gain 2 agenda points")))

(deftest security-testing
  ;; Security Testing
  (do-game
      (new-game {:corp {:deck ["Jackson Howard"]}
                 :runner {:deck ["Security Testing"]}})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (click-prompt state :runner "Server 1")
      (run-empty-server state "Server 1")
      (is (= 10 (:credit (get-runner))) "Gained 2 credits from Security Testing")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 10 (:credit (get-runner))) "Did not gain credits on second run")
      (take-credits state :runner 2)
      (take-credits state :corp)
      (click-prompt state :runner "Server 1")
      (run-empty-server state "Archives")
      (is (= 12 (:credit (get-runner))) "Did not gain credits when running other server")))

(deftest security-testing-with-multiple-copies-on-different-servers
    ;; with multiple copies on different servers
    (do-game
      (new-game {:runner {:deck [(qty "Security Testing" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Security Testing")
      (click-prompt state :runner "Archives")
      (click-prompt state :runner "R&D")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-runner))) "Gained 2 credits")
      (run-empty-server state "R&D")
      (is (= 11 (:credit (get-runner))))))

(deftest security-testing-with-multiple-copies-on-the-same-server
    ;; with multiple copies on the same server
    (do-game
      (new-game {:runner {:deck [(qty "Security Testing" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Security Testing")
      (click-prompt state :runner "Archives")
      (click-prompt state :runner "Archives")
      (run-empty-server state "Archives")
      (click-prompt state :runner "Security Testing")
      (is (= 9 (:credit (get-runner))) "Gained 2 credits")))

(deftest security-testing-with-paragon-and-other-successful-run-triggers
    ;; with Paragon and other successful-run triggers
    (do-game
      (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Sure Gamble"]
                          :hand ["Diversion of Funds"
                                 "Paragon"
                                 "Security Testing"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Paragon")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Archives")
      (play-from-hand state :runner "Diversion of Funds")
      (card-ability state :corp (:identity (get-corp)) 0)
      (click-prompt state :corp "Archives")
      (let [credits (:credit (get-runner))]
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")
        (is (= (+ 3 credits) (:credit (get-runner))) "2 from Sec Testing, 1 from Paragon"))))

(deftest security-testing-with-dirty-laundry-issue-4390
    ;; with Dirty Laundry. Issue #4390
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:hand ["Security Testing" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "Archives")
      (play-from-hand state :runner "Dirty Laundry")
      (click-prompt state :runner "Archives")
      (let [credits (:credit (get-runner))]
        (run-continue state)
        (is (= (+ credits 5 2) (:credit (get-runner))) "Runner gains 5 from Dirty Laundry and 2 from Security Testing"))))

(deftest slipstream-there-is-an-ice-at-the-correct-position
    ;; There is an ice at the correct position
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Ice Wall" 2)]}
                 :runner {:hand ["Slipstream"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Slipstream")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-ice state :rd 0))
      (click-prompt state :runner "No")
      (is (find-card "Slipstream" (:discard (get-runner))) "Slipstream is discarded")
      (is (= :approach-ice (:phase (get-run))) "Run is in approach phase")))

(deftest slipstream-there-isn-t-an-ice-at-the-correct-position
    ;; There isn't an ice at the correct position
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Ice Wall" 3)]
                        :credits 20}
                 :runner {:hand ["Slipstream"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Slipstream")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (run-continue state)
      (is (no-prompt? state :runner) "No Slipstream prompt")))

(deftest slipstream-you-can-only-choose-the-correct-ice
    ;; You can only choose the correct ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Ice Wall" 3)]
                        :credits 20}
                 :runner {:hand ["Slipstream"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Slipstream")
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-ice state :hq 1))
      (is (not (no-prompt? state :runner))
          "Slipstream prompt still up as you can't choose ice at the wrong position")
      (click-card state :runner (get-ice state :hq 0))
      (is (find-card "Slipstream" (:discard (get-runner))) "Slipstream is discarded")
      (click-prompt state :runner "No")
      (is (= :approach-ice (:phase (get-run))) "Run is in approach phase")))

(deftest slipstream-jack-out-after-move
    ;; Jack out after move
    (do-game
     (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                       :hand [(qty "Ice Wall" 2)]}
                :runner {:hand ["Slipstream"]}})
     (play-from-hand state :corp "Ice Wall" "HQ")
     (play-from-hand state :corp "Ice Wall" "R&D")
     (take-credits state :corp)
     (play-from-hand state :runner "Slipstream")
     (run-on state "HQ")
     (rez state :corp (get-ice state :hq 0))
     (run-continue state)
     (run-continue state)
     (click-prompt state :runner "Yes")
     (click-card state :runner (get-ice state :rd 0))
     (click-prompt state :runner "Yes")
     (is (find-card "Slipstream" (:discard (get-runner))) "Slipstream is discarded")
     (is (empty? (get-run)) "Run has ended")))

(deftest slipstream-interaction-with-kakugo
    ;; Interaction with Kakugo
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Kakugo" "Ice Wall"]
                        :credits 20}
                 :runner {:hand ["Slipstream" "Sure Gamble"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Kakugo" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Slipstream")
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-ice state :hq 0))
      (is (find-card "Slipstream" (:discard (get-runner))) "Slipstream is discarded")
      (click-prompt state :runner "No")
      (is (= :approach-ice (:phase (get-run))) "Run is in approach phase")
      (is (find-card "Sure Gamble" (:hand (get-runner))) "Kakugo doesn't deal any net damage")))

(deftest slipstream-interaction-with-spear-phishing
    ;; Interaction with Spear Phishing
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Kakugo" "Ice Wall"]
                        :credits 20}
                 :runner {:hand ["Slipstream" "Spear Phishing" "Sure Gamble"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Kakugo" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Slipstream")
      (play-from-hand state :runner "Spear Phishing")
      (click-prompt state :runner "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-ice state :hq 0))
      (click-prompt state :runner "No")
      (is (= :approach-ice (:phase (get-run))) "Run is in approach ice phase")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= :movement (:phase (get-run))) "Spear Phishing has bypassed Ice Wall")))

(deftest smartware-distributor-basic-functionality
    ;; basic functionality
    (do-game
      (new-game {:runner {:deck ["Smartware Distributor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Smartware Distributor")
      (is (changed? [(get-counters (get-resource state 0) :credit) 3]
            (card-ability state :runner (get-resource state 0) 0))
          "Gains 3 credits")
      (take-credits state :runner)
      (is (changed? [(:credit (get-runner)) 1]
            (take-credits state :corp))
          "Gain 1 credit from Smartware Distributor")
      (is (= 2 (get-counters (get-resource state 0) :credit)) "Smartware Distributor has 2 credits left")))

(deftest spoilers-basic-functionality
    ;; basic functionality
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Hedge Fund"]}
                 :runner {:deck ["Spoilers"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Spoilers")
      (take-credits state :runner)
      (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (is (= 1 (count (:deck (get-corp)))))
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (let [ht (get-content state :remote1 0)]
        (score-agenda state :corp ht)
        (is (= 1 (count (:discard (get-corp)))))
        (is (zero? (count (:deck (get-corp)))) "Last card from R&D milled"))))

(deftest spoilers-interaction-with-friday-chip-issue-4838
    ;; Interaction with Friday Chip. Issue #4838
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hostile Takeover"]}
                 :runner {:deck ["Spoilers" "Friday Chip"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Spoilers")
      (play-from-hand state :runner "Friday Chip")
      (take-credits state :runner)
      (is (changed? [(get-counters (get-hardware state 0) :virus) 0]
            (score-agenda state :corp (get-content state :remote1 0))
            (is (no-prompt? state :runner) "Runner has no Friday Chip prompt"))
          "Friday Chip shouldn't gain counters from Spoilers")))

(deftest stim-dealer
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game {:runner {:deck ["Stim Dealer" "Sure Gamble" "Feedback Filter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "Stim Dealer")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [sd (get-resource state 0)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (get-counters (refresh sd) :power)) "Lost all counters")
      (is (no-prompt? state :runner) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-runner))) "Took 1 core damage")
      (is (= 4 (:click (get-runner))) "Didn't gain extra click"))))

(deftest stoneship-chart-room
  ;; Stoneship Library - Trash to draw 2, trash to charge
  (do-game
    (new-game {:runner {:hand ["Stoneship Chart Room"] :deck [(qty "Sure Gamble" 5)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Stoneship Chart Room")
    (is (changed? [(count (:hand (get-runner))) 2]
          (card-ability state :runner (get-resource state 0) 0))
        "Drew 2 card with stoneship"))
  (do-game
    (new-game {:runner {:hand ["Stoneship Chart Room" "Earthrise Hotel"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Stoneship Chart Room")
    (card-ability state :runner (get-resource state 0) 1)
    (is (= 0 (count (:discard (get-runner)))) "Stoneship not discarded")
    (is (no-prompt? state :runner) "No prompt because stoneship wasn't used")
    (play-from-hand state :runner "Earthrise Hotel")
    (card-ability state :runner (get-resource state 0) 1)
    (click-card state :runner "Earthrise Hotel")
    (is (= 4 (get-counters (get-resource state 0) :power)) "earthrise hotel was charged")))

(deftest street-magic
  ;; Street Magic
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Little Engine"]}
               :runner {:hand ["Street Magic"]}})
    (play-from-hand state :corp "Little Engine" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Magic")
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (card-ability state :runner (get-resource state 0) 0)
    (let [credits (:credit (get-runner))]
      (click-prompt state :runner "Make the Runner gain 5 [Credits]")
      (is (= (+ 5 credits) (:credit (get-runner))) "Runner gained 5 credits"))
    (click-prompt state :runner "End the run")
    (is (not (:run @state)) "Run has ended")))

(deftest street-peddler
  ;; Street Peddler
  (do-game
      (new-game {:runner {:deck ["Street Peddler" "Gordian Blade"
                                 "Torch" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler" "Sure Gamble"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
        (card-ability state :runner sp 0)
        (click-prompt state :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-program state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))

(deftest street-peddler-can-t-afford-install
    ;; Can't afford install
    (do-game
      (new-game {:runner {:deck [(qty "Gordian Blade" 3)]
                          :hand ["Street Peddler"]
                          :credits 0}})
      (take-credits state :corp)
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (card-ability state :runner sp 0)
        (is (no-prompt? state :runner) "no valid targets")
        ;(is (= ["Cancel"] (prompt-buttons :runner)) "1 cancel option on Street Peddler")
        ;(click-prompt state :runner "Cancel") ; choose to install Gordian
        (is (zero? (count (get-program state)))
            "Gordian Blade was not installed")
        (is (and (:installed (refresh sp))
                 (= 3 (count (:hosted (refresh sp)))))
            "Street Peddler still installed with 3 hosted cards"))))

(deftest street-peddler-interaction-with-kate-discount
    ;; Interaction with Kate discount
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Street Peddler"
                                 "Gordian Blade"
                                 (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        ;; should still be able to afford Gordian w/ Kate discount
        (core/lose state :runner :credit 3)
        (card-ability state :runner sp 0)
        (is (= 1 (count (prompt-buttons :runner)))
            "Only 1 choice off Peddler")
        (click-prompt state :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-program state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))

(deftest street-peddler-programs-should-cost-memory-issue-708
    ;; Programs should cost memory. Issue #708
    (do-game
      (new-game {:runner {:deck ["Street Peddler" (qty "Corroder" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (is (= 4 (core/available-mu state)) "No memory cost for hosting on Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
        (card-ability state :runner sp 0)
        (click-prompt state :runner (first (:hosted sp))) ; choose to install Gordian
        (is (= "Corroder" (:title (get-program state 0)))
            "Corroder was installed")
        (is (= 3 (core/available-mu state)) "Corroder cost 1 mu"))))

(deftest street-peddler-muertos-brain-chip-uninstall-effect-not-fired-when-removed-off-peddler-hosting-issue-2294-2358
    ;; Muertos/Brain Chip uninstall effect not fired when removed off peddler/hosting Issue #2294, #2358
    (do-game
      (new-game {:corp {:deck ["Jackson Howard"]}
                 :runner {:deck [(qty "Street Peddler" 2) "Muertos Gang Member" "Brain Chip"]}})
      (core/move state :runner (find-card "Muertos Gang Member" (:hand (get-runner))) :deck {:front true})
      (core/move state :runner (find-card "Brain Chip" (:hand (get-runner))) :deck {:front true})
      (core/move state :runner (find-card "Street Peddler" (:hand (get-runner))) :deck {:front true})
      (play-from-hand state :corp "Jackson Howard" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Street Peddler")
      (core/gain state :runner :agenda-point 1)
      (let [jh (get-content state :remote1 0)
            sp (get-resource state 0)]
        (rez state :corp jh)
        (card-ability state :runner sp 0)
        (click-prompt state :runner (find-card "Street Peddler" (:hosted sp))) ; choose to another Peddler
        (is (no-prompt? state :corp) "Corp not prompted to rez Jackson")
        (is (= 4 (core/available-mu state)) "Runner has 4 MU"))))

(deftest street-peddler-installing-parasite-with-only-1cr-issue-491
    ;; Installing Parasite with only 1cr. Issue #491.
    (do-game
      (new-game {:corp {:deck [(qty "Pop-up Window" 3)]}
                 :runner {:deck ["Street Peddler" "Parasite" "Corroder" "Cache"]}})
      (play-from-hand state :corp "Pop-up Window" "HQ")
      (take-credits state :corp 2)
      (starting-hand state :runner ["Street Peddler"])
      (core/lose state :runner :credit 4) ; go down to 1 credit
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)
            pu (get-ice state :hq 0)]
        (rez state :corp pu)
        (card-ability state :runner sp 0)
        (click-prompt state :runner "Parasite") ; choose to install Parasite
        (is (= "Parasite" (:title (:card (prompt-map :runner))))
            "Parasite target prompt")
        (click-card state :runner pu)
        (is (= 4 (count (:discard (get-runner)))) "3 Parasite, 1 Street Peddler in heap")
        (is (= 1 (count (:discard (get-corp)))) "Pop-up Window in archives"))))

(deftest street-peddler-tech-trader-install
    ;; Tech Trader install
    (do-game
      (new-game {:runner {:deck ["Street Peddler"
                                 "Tech Trader"]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= 1 (count (:hosted sp))) "Street Peddler is hosting 1 card")
        (card-ability state :runner sp 0)
        (click-prompt state :runner (find-card "Tech Trader" (:hosted sp))) ; choose to install Tech Trader
        (is (= "Tech Trader" (:title (get-resource state 0)))
            "Tech Trader was installed")
        (is (= 5 (:credit (get-runner))) "Did not gain 1cr from Tech Trader ability"))))

(deftest street-peddler-the-class-act-being-installed-on-corp-s-turn-issue-4106
    ;; The Class Act being installed on Corp's turn. Issue #4106
    (do-game
      (new-game {:runner {:deck [(qty "Diesel" 10)]
                          :hand ["Street Peddler" "The Class Act"]}})
      (core/move state :runner (find-card "The Class Act" (:hand (get-runner))) :deck {:front true})
      (take-credits state :corp)
      (play-from-hand state :runner "Street Peddler")
      (take-credits state :runner)
      (let [sp (get-resource state 0)]
        (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 card")
        (card-ability state :runner sp 0)
        (click-prompt state :runner "The Class Act")
        (is (= "The Class Act" (:title (get-resource state 0))) "The Class Act was installed on Corp's turn")
        (take-credits state :corp)
        (is (not (no-prompt? state :runner)) "Runner should have The Class Act prompt")
        (is (= "Choose 1 card to add to the bottom of the stack" (-> (prompt-map :runner) :msg))
            "Runner gets The Class Act's power on Corp's turn")
        (click-card state :runner (find-card "Diesel" (:set-aside (get-runner))))
        (play-from-hand state :runner "Diesel")
        (is (= "The Class Act" (-> (prompt-map :runner) :card :title)) "Runner gets The Class Act's power on Runner's turn"))))

(deftest street-peddler-trashed-hosted-cards-are-logged-5024
    ;; Trashed hosted cards are logged. #5024
    (do-game
      (new-game {:runner {:deck ["Gordian Blade" "Torch" "Sure Gamble"]
                          :hand ["Street Peddler"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Street Peddler")
      (let [sp (get-resource state 0)]
        (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
        (card-ability state :runner sp 0)
        (click-prompt state :runner (find-card "Gordian Blade" (:hosted sp)))
        (is (second-last-log-contains? state "are trashed as a result") "The two hosted cards are logged"))))

(deftest symmetrical-visage
  ;; Symmetrical Visage - Gain 1 credit the first time you click to draw each turn
  (do-game
      (new-game {:runner {:deck [(qty "Symmetrical Visage" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Symmetrical Visage")
      (is (= 3 (:credit (get-runner))))
      (click-draw state :runner)
      (is (= 4 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
      (click-draw state :runner)
      (is (= 4 (:credit (get-runner))) "No credit gained from second click spent to draw")))

(deftest symmetrical-visage-gain-1-credit-the-first-and-second-time-you-click-to-draw-each-turn-when-gcs-is-installed
    ;; Gain 1 credit the first and second time you click to draw each turn when GCS is installed
    (do-game
      (new-game {:runner {:deck [(qty "Symmetrical Visage" 3)
                                 (qty "Gene Conditioning Shoppe" 3)
                                 "Fall Guy"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Symmetrical Visage")
      (is (= 3 (:credit (get-runner))))
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (is (= 1 (:credit (get-runner))))
      (click-draw state :runner)
      (is (= 2 (:credit (get-runner))) "Gained 1 credit from first click spent to draw")
      (click-draw state :runner)
      (is (= 3 (:credit (get-runner)))
          "Gained 1 credit from second click spent to draw with Gene Conditioning Shoppe")
      ;; Move Fall Guy back to deck
      (core/move state :runner (find-card "Fall Guy" (:hand (get-runner))) :deck)
      (click-draw state :runner)
      (is (= 3 (:credit (get-runner)))
          "No credit gained from third click spent to draw with Gene Conditioning Shoppe")))

(deftest synthetic-blood
  ;; Synthetic Blood - The first time you take damage each turn, draw one card
  (do-game
      (new-game {:corp {:deck [(qty "Data Mine" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Synthetic Blood" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]}})
      (play-from-hand state :corp "Data Mine" "HQ")
      (play-from-hand state :corp "Data Mine" "HQ")
      (take-credits state :corp)
      (let [first-dm (get-ice state :hq 1)
            second-dm (get-ice state :hq 0)]
        (play-from-hand state :runner "Synthetic Blood")
        (run-on state "HQ")
        (rez state :corp first-dm)
        (run-continue state)
        (card-subroutine state :corp first-dm 0)
        (is (= 4 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
        (run-continue-until state :approach-ice)
        (rez state :corp second-dm)
        (run-continue state)
        (card-subroutine state :corp second-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "no card drawn when receiving damage (2nd time)"))))

(deftest synthetic-blood-the-first-and-second-time-you-take-damage-each-turn-with-gcs-installed-draw-one-card
    ;; The first and second time you take damage each turn (with GCS installed), draw one card
    (do-game
      (new-game {:corp {:deck [(qty "Data Mine" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Synthetic Blood" 3)
                                 "Sure Gamble"
                                 (qty "Gene Conditioning Shoppe" 3)]}})
      (play-from-hand state :corp "Data Mine" "HQ")
      (play-from-hand state :corp "Data Mine" "HQ")
      (take-credits state :corp)
      (let [first-dm (get-ice state :hq 1)
            second-dm (get-ice state :hq 0)]
        (play-from-hand state :runner "Synthetic Blood")
        (play-from-hand state :runner "Gene Conditioning Shoppe")
        (run-on state "HQ")
        (rez state :corp first-dm)
        (run-continue state)
        (card-subroutine state :corp first-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (1st time)")
        (run-continue-until state :approach-ice)
        (rez state :corp second-dm)
        (run-continue state)
        (card-subroutine state :corp second-dm 0)
        (is (= 3 (count (:hand (get-runner)))) "1 card drawn when receiving damage (2nd time)"))))

(deftest tech-trader
  ;; Basic test
  (do-game
    (new-game {:runner {:deck ["Tech Trader" "Fall Guy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Tech Trader")
    (play-from-hand state :runner "Fall Guy")
    (is (= 4 (:credit (get-runner))))
    (let [fall (get-resource state 1)]
      (card-ability state :runner fall 1)
      (is (= 7 (:credit (get-runner)))))))

(deftest technical-writer
  ;; Technical Writer - Gain 1c per program/hardware install; click/trash to take all credits
  (do-game
      (new-game {:runner {:deck ["Technical Writer" (qty "Faerie" 2)
                                 "Vigil" "Same Old Thing"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 2)
      (play-from-hand state :runner "Technical Writer")
      (let [tw (get-resource state 0)]
        (play-from-hand state :runner "Faerie")
        (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
        (play-from-hand state :runner "Faerie")
        (is (= 2 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
        (play-from-hand state :runner "Vigil")
        (is (= 3 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
        (play-from-hand state :runner "Same Old Thing")
        (is (= 3 (get-counters (refresh tw) :credit)) "No credit gained for resource install")
        (card-ability state :runner tw 0)
        (is (= 6 (:credit (get-runner))) "Gained 3 credits")
        (is (zero? (:click (get-runner))) "Spent 1 click")
        (is (= 1 (count (:discard (get-runner)))) "Technical Writer trashed"))))

(deftest technical-writer-interaction-with-facedown-cards-issue-4498
    ;; Interaction with facedown cards. Issue #4498
    (do-game
      (new-game {:runner {:hand ["Technical Writer" "Aesop's Pawnshop" "Harbinger"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Technical Writer")
      (play-from-hand state :runner "Aesop's Pawnshop")
      (play-from-hand state :runner "Harbinger")
      (let [tw (get-resource state 0)]
        (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
        (take-credits state :runner)
        (take-credits state :corp)
        (card-ability state :runner (get-resource state 1) 0)
        (click-card state :runner (get-program state 0))
        (is (= 1 (count (get-runner-facedown state))) "Harbinger is facedown")
        (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 0c from Harbinger installing facedown"))))

(deftest telework-contract
  ;; Telework Contract
  (do-game
      (new-game {:runner {:deck ["Telework Contract"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Telework Contract")
      (is (changed? [(:credit (get-runner)) 3]
            (card-ability state :runner (get-resource state 0) 0))
          "Got 3 credits from Telework Contract")
      (is (changed? [(:credit (get-runner)) 0]
            (card-ability state :runner (get-resource state 0) 0))
          "Got 0 credits from second attempt to click Telework Contract")
      (is (= 2 (:click (get-runner))) "No click taken from second attempt")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner (get-resource state 0) 0)
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner (get-resource state 0) 0)
      (is (empty? (get-resource state)) "Telework Contract trashed after all credits taken")))

(deftest temujin-contract-multiple-times-in-one-turn-issue-1952
    ;; Multiple times in one turn. Issue #1952
    (do-game
      (new-game {:runner {:id "Silhouette: Stealth Operative"
                          :deck ["Temüjin Contract"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "Archives")
      (run-empty-server state "Archives")
      (is (= 5 (:credit (get-runner))) "Gained 4cr")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "Temjin has 12 credits remaining")))

(deftest the-archivist
  ;; The Archivist
  (do-game
    (new-game {:corp {:deck ["Global Food Initiative" "Private Security Force"]}
               :runner {:deck ["The Archivist"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Archivist")
    (is (zero? (count-bad-pub state)) "Corp should start with 0 bad publicity")
    (take-credits state :runner)
    (play-and-score state "Global Food Initiative")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (count-bad-pub state)) "Corp should get 1 bad publicity from The Archivist")
    (play-and-score state "Private Security Force")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 2 (count-bad-pub state)) "Corp should get 1 bad publicity from The Archivist")))

(deftest the-artist
  ;; The Artist
  (do-game
    (new-game {:runner {:deck ["The Artist" "Bankroll" "Bankroll"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Artist")
    (is (empty? (get-program state)) "No programs installed")
    (is (= 1 (:credit (get-runner))))
    (let [artist (get-resource state 0)]
      (card-ability state :runner artist 0)
      (is (= 2 (:click (get-runner))))
      (is (= 3 (:credit (get-runner))) "Gain 2cr from Artist")
      (card-ability state :runner artist 0)
      (is (= 2 (:click (get-runner))))
      (is (= 3 (:credit (get-runner))) "Second use of Artist for creds not allowed")
      (card-ability state :runner artist 1)
      (click-card state :runner (find-card "Bankroll" (:hand (get-runner))))
      (is (= 1 (:click (get-runner))))
      (is (= 1 (count (get-program state))) "1 Program installed")
      (is (= 3 (:credit (get-runner))) "Artist discount applied")
      (card-ability state :runner artist 1)
      (is (no-prompt? state :runner) "Second use of Artist for install not allowed")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner artist 0)
      (is (= 3 (:click (get-runner))))
      (is (= 6 (:credit (get-runner))) "Gain 2cr from Artist new turn")
      (card-ability state :runner artist 1)
      (click-card state :runner (find-card "Bankroll" (:hand (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (= 2 (count (get-program state))) "2 Programs installed")
      (is (= 6 (:credit (get-runner))) "Artist discount applied new turn"))))

(deftest the-back-happy-path
    ;; Happy Path
    (do-game
      (new-game {:runner {:hand    ["The Back"]
                          :discard ["Spy Camera" "Recon Drone" "All-nighter" "Deus X"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Back")
      (let [the-back (get-resource state 0)]
        (core/add-counter state :runner (refresh the-back) :power 2)
        (card-ability state :runner (refresh the-back) 1)
        (is (= "Choose up to 4 targets for The Back" (:msg (prompt-map :runner))) "Runner gets up to 4 cards")
        (click-card state :runner "Spy Camera")             ; Hardware
        (click-card state :runner "Recon Drone")            ; Hardware with :trash-icon
        (click-card state :runner "Deus X")                 ; Program
        (click-card state :runner "All-nighter")            ; Resource
        (is (find-card "Spy Camera" (:deck (get-runner))) "Spy Camera is shuffled back into the stack")
        (is (find-card "Recon Drone" (:deck (get-runner))) "Program is shuffled back into the stack")
        (is (find-card "Deus X" (:deck (get-runner))) "Deus X is shuffled back into the stack")
        (is (find-card "All-nighter" (:deck (get-runner))) "All-nighter is shuffled back into the stack"))))

(deftest the-back-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:hand ["Blacklist"]}
                 :runner {:hand    ["The Back"]
                          :discard ["Spy Camera" "Recon Drone" "All-nighter" "Deus X"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "The Back")
      (let [the-back (get-resource state 0)]
        (core/add-counter state :runner (refresh the-back) :power 2)
        (card-ability state :runner (refresh the-back) 1)
        (is (no-prompt? state :runner) "The Back prompt did not come up"))))

#_(deftest ^:kaocha/pending the-back-automated
  ;; TODO: Enable this once card is fully implemented
  (testing "Basic test"
    (do-game
      (new-game {:runner {:hand ["The Back" (qty "Spy Camera" 2) "Lockpick" "Refractor"]
                          :discard [(qty "Spy Camera" 4) "Sure Gamble" "All-nighter" "Daily Casts" "Bankroll"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Back")
      (play-from-hand state :runner "Lockpick")
      (play-from-hand state :runner "Refractor")
      (let [tb (get-resource state 0)
            lp (get-hardware state 0)
            refr (get-program state 0)]
        (is (= 0 (get-counters (refresh tb) :power)) "No counters on The Back at start")
        (run-on state :hq)
        (card-ability state :runner refr 1)
        (click-card state :runner lp)
        (is (= 1 (get-counters (refresh tb) :power)) "Added 1 counter to The Back")
        (run-jack-out state))
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Spy Camera")
      (let [tb (get-resource state 0)
            sc (get-hardware state 1)]
        (run-on state :hq)
        (card-ability state :runner sc 1)
        (card-ability state :runner tb 0)
        (is (= 2 (get-counters (refresh tb) :power)) "Manually added 1 counter to The Back")
        (click-prompt state :runner "OK")
        (run-jack-out state)
        (let [heapsize (count (:discard (get-runner)))]
          (card-ability state :runner tb 1)
          (is (str/starts-with? (:msg (prompt-map :runner)) "Choose up to 4") "Runner gets up to 4 choices")
          (click-card state :runner (find-card "Spy Camera" (:discard (get-runner))))
          (click-card state :runner (find-card "Bankroll" (:discard (get-runner))))
          (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
          (click-card state :runner (find-card "All-nighter" (:discard (get-runner))))
          (click-card state :runner (find-card "Daily Casts" (:discard (get-runner))))
          (click-prompt state :runner "Done")
          (is (= (- heapsize 3) (count (:discard (get-runner)))) "Selected three of those cards to shuffle back")
          (is (= 1 (count (:rfg (get-runner)))) "The Back removed from game"))))))

(deftest the-black-file
  ;; The Black File - Prevent Corp from winning by agenda points
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Vanity Project" "Degree Mill"]}
                 :runner {:deck ["The Black File"]}})
      (play-and-score state "Degree Mill")
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (take-credits state :runner)
      (play-and-score state "Vanity Project")
      (is (= 7 (:agenda-point (get-corp))))
      (is (not (:winner @state)) "No registered Corp win")
      (take-credits state :corp)
      (let [bf (get-resource state 0)]
        (is (= 1 (get-counters (refresh bf) :power)) "1 power counter on The Black File")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh bf) :power)) "2 power counters on The Black File")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:rfg (get-runner)))) "The Black File removed from the game")
        (is (= :corp (:winner @state)) "Corp wins")
        (is (= "Agenda" (:reason @state)) "Win condition reports agendas"))))

(deftest the-black-file-corp-can-still-win-by-flatlining-runner
    ;; Corp can still win by flatlining Runner
    (do-game
      (new-game {:corp {:deck [(qty "Vanity Project" 3) (qty "Scorched Earth" 3)]
                        :hand ["Vanity Project" "Scorched Earth" "Degree Mill"]}
                 :runner {:deck ["The Black File"]}})
      (play-and-score state "Degree Mill")
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (take-credits state :runner)
      (play-and-score state "Vanity Project")
      (is (= 7 (:agenda-point (get-corp))))
      (is (not (:winner @state)) "No registered Corp win")
      (take-credits state :corp)
      (take-credits state :runner)
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest the-black-file-trash-effect-works-correctly
    ;; Trash effect works correctly
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Vanity Project" "Degree Mill"]}
                 :runner {:deck ["The Black File"]}})
      (play-and-score state "Degree Mill")
      (take-credits state :corp)
      (play-from-hand state :runner "The Black File")
      (take-credits state :runner)
      (play-and-score state "Vanity Project")
      (is (= 7 (:agenda-point (get-corp))))
      (is (not (:winner @state)) "No registered Corp win")
      (gain-tags state :runner 1)
      (trash-card state :runner (get-resource state 0))
      (is (= :corp (:winner @state)) "Corp has now won")))

(deftest the-class-act-vanilla-test
    ;; Vanilla test
    (do-game
     (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                         :hand ["The Class Act" "Easy Mark" "Corporate Defector"]}})
     (core/move state :runner (find-card "Easy Mark" (:hand (get-runner))) :deck {:front true}) ;ensure easy mark is on the top
     (is (= "Easy Mark" (-> (get-runner) :deck first :title)) "Easy Mark is on top of deck")
     (take-credits state :corp)
     (play-from-hand state :runner "The Class Act")
     (is (empty? (get-program state)) "No programs installed")
     (is (= 1 (:credit (get-runner))))
     (is (no-prompt? state :runner) "The Class Act has done nothing yet, so there is no Runner prompt")
     (is (no-prompt? state :corp) "The Class Act has done nothing yet, so there is no Corp prompt")
     (is (= 6 (count (:deck (get-runner)))) "4 cards in deck to be drawn")
     (take-credits state :runner)
     (is (= :select (prompt-type :runner))
       "The Class Act triggered its draw ability, so Runner needs to choose")
     (is (= :waiting (prompt-type :corp))
       "The Class Act triggered its draw ability, so Corp must wait while Runner chooses")
     (is (not= "Easy Mark" (-> (get-runner) :deck last :title)) "Easy Mark is not on the bottom of the deck yet")
     (click-card state :runner "Easy Mark")
     (is (no-prompt? state :corp) "The Class Act has bottomed a card, so there is no Corp prompt")
     (is (no-prompt? state :runner) "The Class Act has has bottomed a card, so there is no Runner prompt")
     (is (= "Easy Mark" (-> (get-runner) :deck last :title)) "Easy Mark was put on bottom of deck")
     (is (= 5 (count (:hand (get-runner)))))
     (is (= 2 (count (:deck (get-runner)))))
     (take-credits state :corp)
     (take-credits state :runner)
     (is (= 2 (count (:deck (get-runner)))) "The Class Act does not trigger at the end of a turn it wasn't installed, so no cards were drawn")
     (is (no-prompt? state :runner) "The Class Act does not trigger at the end of a turn it wasn't installed, so there is no prompt")
     (take-credits state :corp)
     (click-draw state :runner)
     (is (= :select (prompt-type :runner))
       "The Class Act is prompting the runner to choose")
     (is (= :waiting (prompt-type :corp))
       "The Class Act is insisting the corp waits")
     (click-card state :runner "Easy Mark")
     (is (no-prompt? state :runner) "The Class Act is no longer prompting the runner to choose")
     (is (no-prompt? state :corp) "The Class Act is no longer insisting the corp waits")
     (is (= "Easy Mark" (-> (get-runner) :deck last :title)) "Easy Mark was bottomed again")
     (click-draw state :runner)
     (is (zero? (count (:deck (get-runner)))) "Runner only drew one")
     (is (no-prompt? state :runner) "The Class Act did not trigger")
     (is (no-prompt? state :corp) "The Class Act is no longer insisting the corp waits")))

(deftest the-class-act-interaction-with-other-sources-of-draw
    ;; Interaction with other sources of draw
    (do-game
     (new-game {:runner {:deck [(qty "Sure Gamble" 3)]
                         :hand ["The Class Act" "Laguna Velasco District"]}})
     (take-credits state :corp)
     (core/gain state :runner :credit 10)
     (play-from-hand state :runner "The Class Act")
     (play-from-hand state :runner "Laguna Velasco District")
     (is (= 3 (count (:deck (get-runner)))) "3 cards in deck")
     (click-draw state :runner)
     (is (not (no-prompt? state :runner)) "The Class Act is prompting the runner to choose")
     (is (not (no-prompt? state :corp)) "The Class Act is insisting the corp waits")
     (click-card state :runner (find-card "Sure Gamble" (:set-aside (get-runner))))
     (is (no-prompt? state :runner) "The Class Act is done prompting the runner to choose")
     (is (no-prompt? state :corp) "The Class Act is not insisting the corp waits")
     (is (= 1 (count (:deck (get-runner)))) "1 card put back")))

(deftest the-class-act-ensure-draw-filtering-is-properly-awaited
    ;; Ensure draw filtering is properly awaited
    (do-game
     (new-game {:runner {:deck [(qty "Sure Gamble" 3)]
                         :hand ["The Class Act" "Paragon" "John Masanori"]
                         :credits 10}})
     (take-credits state :corp)
     (play-from-hand state :runner "The Class Act")
     (play-from-hand state :runner "Paragon")
     (play-from-hand state :runner "John Masanori")
     (is (= 3 (count (:deck (get-runner)))) "3 cards in deck")
     (run-empty-server state "Archives")
     (click-prompt state :runner "John Masanori") ; runner should be prompted for which to trigger first
     (is (= 2 (count (:prompt (get-runner)))) "The Class Act is prompting the runner to choose, but Paragon prompt is not open yet")
     (is (not (no-prompt? state :corp)) "The Class Act is insisting the corp waits")
     (click-card state :runner (find-card "Sure Gamble" (:set-aside (get-runner))))
     (is (= 2 (count (:deck (get-runner)))) "The Class Act put a card back")
     (is (changed? [(:credit (get-runner)) 1]
           (click-prompt state :runner "Yes")
           ; runner prompted to trigger Paragon
           (click-prompt state :runner "Yes")))
     (is (no-prompt? state :runner) "The Class Act is done prompting the runner to choose")
     (is (no-prompt? state :corp) "The Class Act is not insisting the corp waits")
     (is (= 2 (count (:deck (get-runner)))) "Deck still has 2 cards")))

(deftest the-class-act-no-trigger-when-drawing-zero-cards
  ;; no trigger when drawing zero cards
  (do-game
   (new-game {:runner {:deck [(qty "Sure Gamble" 3)]
                       :hand ["The Class Act", "Obelus", "Diversion of Funds"]
                       :credits 10}})
   (take-credits state :corp)
   (play-from-hand state :runner "The Class Act")
   (play-from-hand state :runner "Obelus")
   (play-run-event state "Diversion of Funds" :hq)
   (click-prompt state :runner "Diversion of Funds")
   (is (no-prompt? state :runner) "No prompt from The Class Act")
   (is (empty? (find-card "Sure Gamble" (:hand (:runner @state)))) "Sure Gamble has not been drawn")))

(deftest the-class-act-no-trigger-when-drawing-one-card
  ;; no trigger when drawing one card
  (do-game
   (new-game {:runner {:deck ["Sure Gamble"]
                       :hand ["The Class Act"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "The Class Act")
   (click-draw state :runner)
   (is (no-prompt? state :runner) "No prompt from The Class Act")))

(deftest the-class-act-no-lingering-bonus-draw-effect-if-no-cards-in-deck
  ;; The Class Act - Issue #6132 - No lingering bonus draw effect if no cards in deck
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
              :runner {:hand ["The Class Act", "Obelus"]
                       :credits 10}})
   (take-credits state :corp)
   (click-card state :corp (first (:hand (get-corp))))
   (play-from-hand state :runner "The Class Act")
   (play-from-hand state :runner "Obelus")
   (run-empty-server state :hq)
   (click-prompt state :runner "No action")
   (is (changed? [(count (:hand (get-corp))) 1]
         (take-credits state :runner))
       "Draw 1 card at the start of turn")))

(deftest the-class-act-draw-bonus
  ;; The Class Act - Issue #7182 - Not receiving bonus draw from Verbal Plasticity
  (do-game
    (new-game {:runner {:deck [(qty "Sure Gamble" 10)]
                        :hand ["The Class Act" "Verbal Plasticity"]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Verbal Plasticity")
    (play-from-hand state :runner "The Class Act")
    (click-draw state :runner)
    (is (not (no-prompt? state :runner)) "The Class Act is prompting the runner to choose")
    (is (= 3 (count (:set-aside (get-runner)))) "The Class Act set aside 3 cards")))

(deftest the-helpful-ai
  ;; The Helpful AI - +1 link; trash to give an icebreaker +2 str until end of turn
  (do-game
    (new-game {:runner {:deck ["The Helpful AI" "Corroder"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Helpful AI")
    (is (= 1 (get-link state)) "Gained 1 link")
    (play-from-hand state :runner "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :runner (get-resource state 0) 0)
      (click-card state :runner corr)
      (is (= 4 (get-strength (refresh corr))) "Corroder has +2 strength")
      (is (= 1 (count (:discard (get-runner)))) "Helpful AI trashed")
      (is (zero? (get-link state)))
      (take-credits state :runner)
      (is (= 2 (get-strength (refresh corr))) "Corroder back to default strength"))))

(deftest the-nihilist-vanilla-test
    ;; Vanilla test
    (do-game
     (new-game {:runner {:deck ["The Nihilist" (qty "Cache" 2) (qty "Sure Gamble" 3)]}
                :corp {:deck [(qty "Ice Wall" 10)]}})
     (starting-hand state :runner ["The Nihilist" "Cache" "Cache"])
     (starting-hand state :corp [])
     (take-credits state :corp)
     (core/gain state :runner :credit 10)
     (play-from-hand state :runner "The Nihilist")
     (play-from-hand state :runner "Cache")
     (let [nihi (get-resource state 0)
           cache1 (get-program state 0)]
       (is (= 2 (get-counters (refresh nihi) :virus)) "The Nihilist gained 2 counters")
       (play-from-hand state :runner "Cache")
       (is (= 2 (get-counters (refresh nihi) :virus)) "The Nihilist only triggered on first install")
       (take-credits state :runner)
       (take-credits state :corp)
       (click-prompt state :runner "Yes")  ; spend 2 tokens?
       (is (= 2 (get-counters (refresh nihi) :virus)) "The Nihilist has 2 counters")
       (is (= 3 (get-counters (refresh cache1) :virus)) "Cache 1 has 3 counters")
       (click-card state :runner (refresh nihi))
       (click-card state :runner (refresh cache1))
       (is (= 1 (get-counters (refresh nihi) :virus)) "The Nihilist spent 1 counter")
       (is (= 2 (get-counters (refresh cache1) :virus)) "Cache 1 spent 1")
       (is (not (no-prompt? state :runner)) "Runner is waiting for Corp to pick their Nihilist poison")
       (is (= 0 (count (:discard (get-corp)))) "No cards in Archives")
       (click-prompt state :corp "Trash the top card of R&D")  ; mill 1
       (is (= 1 (count (:discard (get-corp)))) "1 card milled")
       (is (no-prompt? state :runner) "Runner is done waiting for Corp to pick their Nihilist poison")
       (is (no-prompt? state :corp) "No more Corp prompts")
       (take-credits state :runner)
       (take-credits state :corp)
       (click-prompt state :runner "Yes")  ; spend 2 tokens?
       (is (= 1 (get-counters (refresh nihi) :virus)) "The Nihilist has 1 counters")
       (is (= 2 (get-counters (refresh cache1) :virus)) "Cache 1 has 2 counters")
       (click-card state :runner (refresh nihi))
       (click-card state :runner (refresh cache1))
       (is (= 0 (get-counters (refresh nihi) :virus)) "The Nihilist spent 1 counter")
       (is (= 1 (get-counters (refresh cache1) :virus)) "Cache 1 spent 1")
       (is (= 0 (count (:hand (get-runner)))) "Runner has no cards in hand")
       (is (= 1 (count (:discard (get-corp)))) "1 card in discard")
       (is (not (no-prompt? state :runner)) "Runner is waiting for Corp to pick their Nihilist poison")
       (click-prompt state :corp "The Runner draws 2 cards")  ; don't mill 1
       (is (= 2 (count (:hand (get-runner)))) "Runner drew 2 cards")
       (is (= 1 (count (:discard (get-corp)))) "No extra cards milled")
       (is (no-prompt? state :runner) "Runner done waiting for Corp to pick their Nihilist poison")
       (is (no-prompt? state :corp) "Corp has no more prompts")
       (take-credits state :runner)
       (purge state :corp)
       (take-credits state :corp)
       (is (not (no-prompt? state :runner)) "Runner gets a prompt cuz we don't know what they have"))))

(deftest the-nihilist-cannot-remove-virus-counters-from-corp-cards
    ;; Cannot remove virus counters from Corp cards
    (do-game
      (new-game {:runner {:deck ["The Nihilist" "Cache"]}
                 :corp {:deck [(qty "Sandstone" 10)]}})
      (starting-hand state :runner ["The Nihilist" "Cache"])
      (starting-hand state :corp ["Sandstone"])
      (play-from-hand state :corp "Sandstone" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "The Nihilist")
      (play-from-hand state :runner "Cache")
      (let [sandstone (get-ice state :hq 0)]
        (rez state :corp sandstone)
        (run-on state "HQ")
        (run-continue state)
        (card-subroutine state :corp sandstone 0)
        (is (not (:run @state)) "Run Ended")
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes") ; spend 2 tokens?
        (is (= 1 (get-counters (refresh sandstone) :virus)) "Sandstone has 1 virus counter")
        (click-card state :runner (refresh sandstone))
        (is (= 1 (get-counters (refresh sandstone) :virus)) "Sandstone should still have 1 virus counter"))))

(deftest the-nihilist-ability-activation-trigger-only-counts-runner-virus-counters
    ;; Ability activation trigger only counts Runner virus counters
    (do-game
      (new-game {:runner {:deck ["The Nihilist"]}
                 :corp {:deck ["Sandstone"]}})
      (starting-hand state :runner ["The Nihilist"])
      (starting-hand state :corp ["Sandstone"])
      (play-from-hand state :corp "Sandstone" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "The Nihilist")
      (take-credits state :runner)
      (let [sandstone (get-ice state :hq 0)
            nihilist (get-resource state 0)]
        (rez state :corp sandstone)
        (purge state :corp)
        (core/add-counter state :corp sandstone :virus 2)
        (is (= 0 (get-counters (refresh nihilist) :virus)) "The Nihilist has 0 virus counters")
        (is (= 2 (get-counters (refresh sandstone) :virus)) "Sandstone has 2 virus counters")
        (is (= 0 (count (prompt-buttons :runner))) "The Nihilist did not trigger"))))

(deftest the-shadow-net-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["The Shadow Net" "Sure Gamble" "Easy Mark" "Titanium Ribs"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Shadow Net")
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "Shadow Net prompt did not come up, no agenda and no target")
      (run-empty-server state :hq)
      (click-prompt state :runner "Steal")
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "Shadow Net prompt did not come up, no target")
      (play-from-hand state :runner "Titanium Ribs")
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 4 (:credit (get-runner))) "Paid 1c to install ribs")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "Sure Gamble")
      (click-card state :runner (get-scored state :runner 0))
      (is (= 13 (:credit (get-runner))) "Did't have to pay for Sure Gamble")
      (take-credits state :corp)
      (click-credit state :runner)
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "Shadow Net prompt did not come up, no agenda")))

(deftest the-shadow-net-heap-locked
    ;; Heap locked
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Blacklist"]}
                 :runner {:deck ["The Shadow Net" "Sure Gamble" "Easy Mark" "Titanium Ribs"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "The Shadow Net")
      (run-empty-server state :hq)
      (click-prompt state :runner "Steal")
      (play-from-hand state :runner "Titanium Ribs")
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 4 (:credit (get-runner))) "Paid 1c to install ribs")
      (card-ability state :runner (get-resource state 0) 0)
      (is (no-prompt? state :runner) "Shadow Net prompt did not come up, heap is locked")))

(deftest the-source
  ;; The Source - Increase advancement requirement of agendas by 1; 3c additional cost to steal
  (do-game
    (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
               :runner {:deck [(qty "The Source" 2) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "The Source")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay to steal") ; pay 3c extra to steal
    (is (= 4 (:credit (get-runner))) "Paid 3c to steal")
    (is (= 2 (count (:discard (get-runner)))) "The Source is trashed")
    (play-from-hand state :runner "The Source")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote2 0)]
      (click-advance state :corp (refresh ht))
      (click-advance state :corp (refresh ht))
      (score state :corp (refresh ht))
      (is (empty? (:scored (get-corp))) "Hostile Takeover can't be scored with 2 adv")
      (core/gain state :corp :click 1)
      (click-advance state :corp (refresh ht))
      (score state :corp (refresh ht))
      (is (= 1 (:agenda-point (get-corp))) "Hostile Takeover scored with 3 adv")
      (is (= 3 (count (:discard (get-runner)))) "The Source is trashed"))))

(deftest the-source-trash-does-not-trigger-dummy-box
  ;; The Source trash doesn't trigger Dummy Box
  (do-game
    (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
               :runner {:deck ["Sure Gamble" (qty "The Source" 3) "Dummy Box"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "The Source")
    (play-from-hand state :runner "Dummy Box")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay to steal")
    (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trash")
    (play-from-hand state :runner "The Source")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote2 0)]
      (click-advance state :corp (refresh ht))
      (click-advance state :corp (refresh ht))
      (core/gain state :corp :click 1)
      (click-advance state :corp (refresh ht))
      (score state :corp (refresh ht))
      (is (no-prompt? state :runner) "Dummy Box not prompting to prevent trash"))))

(deftest the-supplier
  ;; The Supplier
  (do-game
      (new-game {:runner {:deck ["The Supplier"
                                 "Plascrete Carapace"
                                 "Utopia Shard"
                                 "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (click-card state :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
        (card-ability state :runner ts 0)
        (is (= 1 (count (prompt-buttons :runner))))
        (click-card state :runner (find-card "Utopia Shard" (:hand (get-runner))))
        (is (= 2 (count (:hosted (refresh ts)))) "The Supplier is hosting 2 cards")
        (take-credits state :runner)
        (take-credits state :corp)
        ;; Utopia Shard cannot be afforded and should not be in the prompt
        (click-card state :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (= 2 (:credit (get-runner)))
            "Runner charged 1 credit to install Plascrete off The Supplier")
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))) "Runner ends turn with 5 credits")
        (is (= 1 (count (:hosted (refresh ts)))) "One card still on The Supplier"))))

(deftest the-supplier-interaction-with-kate-discount-issue-578
    ;; Interaction with Kate discount. Issue #578.
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["The Supplier"
                                 "Plascrete Carapace"
                                 "Kati Jones"
                                 "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (click-card state :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
        (core/lose state :runner :credit (:credit (get-runner)))
        (end-turn state :runner)
        (take-credits state :corp)
        (click-card state :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (zero? (:credit (get-runner))) "Kate discount applied")
        (is (= 1 (count (get-resource state))) "Plascrete installed"))))

(deftest the-supplier-brain-chip-mem-is-deducted-when-it-is-hosted-and-supplier-is-trashed-issue-2358
    ;; Brain chip mem is deducted when it is hosted and Supplier is trashed. Issue #2358
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
                 :runner {:deck ["The Supplier"
                                 "Brain Chip"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (is (= 4 (core/available-mu state)) "Runner has 4 MU")
      (play-from-hand state :runner "The Supplier")
      (let [ts (get-resource state 0)]
        (card-ability state :runner ts 0)
        (click-card state :runner (find-card "Brain Chip" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "Runner has 4 MU")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (take-credits state :runner)
        (gain-tags state :runner 1)
        (trash-resource state)
        (click-card state :corp (get-resource state 0))
        (is (= 2 (count (:discard (get-runner)))))
        (is (= 4 (core/available-mu state)) "Runner has 4 MU"))))

(deftest the-turning-wheel
  ;; The Turning Wheel
  (let [ttw-test
          (fn [server idx kw]
            (do-game
              (new-game {:corp {:deck ["Hostile Takeover" "Ice Wall" "Ice Wall"]}
                         :runner {:deck ["The Turning Wheel"]}})
              (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
              (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
              (take-credits state :corp)
              (play-from-hand state :runner "The Turning Wheel")
              (core/gain state :runner :click 10 :credit 10)
              (let [ttw (get-resource state 0)]
                (run-empty-server state server)
                (click-prompt state :runner "No action")
                (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
                (run-empty-server state server)
                (click-prompt state :runner "No action")
                (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
                (run-on state server)
                (card-ability state :runner ttw idx)
                (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
                (run-continue state)
                (is (= 1 (core/access-bonus-count state :runner kw)) "Runner should access 1 additional card"))))]
      (ttw-test "R&D" 0 :rd)
      (ttw-test "HQ" 1 :hq)))

(deftest the-turning-wheel-access-bonus-shouldn-t-carry-over-to-other-runs-if-prematurely-ended-after-spending-ttw-counters-3598
    ;; Access bonus shouldn't carry over to other runs if prematurely ended after spending TTW counters. #3598
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Nisei MK II"]}
                 :runner {:deck ["The Turning Wheel"]}})
      (play-and-score state "Nisei MK II")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)))
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (core/gain state :runner :click 10 :credit 10)
      (let [nisei (get-scored state :corp 0)
            ttw (get-resource state 0)]
        (run-empty-server state "HQ")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "HQ")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "R&D")
        (card-ability state :runner ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (card-ability state :corp nisei 0)
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter from corp using Nisei counter")
        (run-empty-server state "R&D")
        (is (zero? (core/access-bonus-count state :runner :rd)) "Access bonus should be reset on new run"))))

(deftest the-turning-wheel-spending-counters-shouldn-t-increase-accesses-when-running-a-non-r-d-hq-server
    ;; Spending counters shouldn't increase accesses when running a non-R&D/HQ server
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Ice Wall"]}
                 :runner {:deck ["The Turning Wheel"]}})
      (core/move state :corp (find-card "Ice Wall" (:hand (get-corp))) :deck)
      (trash-from-hand state :corp "Hostile Takeover")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (core/gain state :runner :click 10 :credit 10)
      (let [ttw (get-resource state 0)]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "Archives")
        (card-ability state :runner ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (run-continue state)
        (is (zero? (core/access-bonus-count state :runner :rd)) "Access bonuses are zeroed out when attacked server isn't R&D or HQ")
        (click-prompt state :runner "Steal"))))

(deftest the-turning-wheel-a-given-ability-shouldn-t-give-accesses-when-running-the-other-server
    ;; A given ability shouldn't give accesses when running the other server
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 5)]
                        :hand [(qty "Fire Wall" 5)]}
                 :runner {:hand ["The Turning Wheel"]
                          :credits 10}})
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "The Turning Wheel")
      (let [ttw (get-resource state 0)]
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "HQ")
        (card-ability state :runner ttw 0) ;; The R&D access ability
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (run-continue state)
        (is (zero? (core/access-bonus-count state :runner :hq)) "Runner should access 0 additional card")
        (is (accessing state "Fire Wall"))
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "Runner should have no more access prompts available"))))

(deftest the-turning-wheel-bounce-test
    ;; Bounce test
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 5)]
                        :hand [(qty "Hard-Hitting News" 5)]}
                 :runner {:hand ["The Turning Wheel"]
                          :credits 10}})
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "The Turning Wheel")
      (let [ttw (get-resource state 0)]
        (card-ability state :runner ttw 2) ;; Bounce HQ ability
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel ability has 1 power counter")
        (card-ability state :runner ttw 3) ;; Bounce R&D ability
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel ability has 2 power counter")
        (take-credits state :runner)
        (play-from-hand state :corp "Hard-Hitting News")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 4 (count-tags state)) "Bouncing enables Hard-Hitting News"))))

(deftest the-twinning-net-mercur-corp-turn
  (do-game
    (new-game {:runner {:hand ["The Twinning" "Net Mercur", "Mantle", "Corroder" "Spec Work"] :credits 15}
               :corp {:hand [(qty "Hedge Fund" 4)] :deck [(qty "Hedge Fund" 4)]}})
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (play-from-hand state :runner "Net Mercur")
    (play-from-hand state :runner "Mantle")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "The Twinning")
    (let [corr (get-program state 1)
          mantle (get-program state 0)]
      (run-on state :hq)
      (is (changed? [(:credit (get-runner)) 0]
            (card-ability state :runner corr 1)
            (click-card state :runner mantle)
            (click-prompt state :runner "Place 1 [Credits] on Net Mercur"))
          "Used 1 credit from mantle")
      (run-jack-out state)
      (play-from-hand state :runner "Spec Work")
      (click-card state :runner "Mantle")
      (take-credits state :runner)
      (play-from-hand state :corp "Hedge Fund")
      (let [twin (get-resource state 1)]
        (is (changed? [(get-counters (refresh twin) :power) 1]
              (card-ability state :runner corr 1)
              (click-card state :runner "Net Mercur"))
            "1 counter for spending on the corp turn")))
  ))

(deftest the-twinning
  ;; First time each turn you spend credits from an installed card, place 1 power counter
  ;; on breach hq/r&d - spend up to 2 counters to access that many more cards
  (do-game
    (new-game {:runner {:hand ["The Twinning" "Sahasrara" "Cache" "Cache"] :credits 10}
               :corp {:hand [(qty "Hedge Fund" 4)] :deck [(qty "Hedge Fund" 4)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Twinning")
    (play-from-hand state :runner "Sahasrara")
    (let [twin (get-resource state 0)]
      (is (changed? [(get-counters (refresh twin) :power) 1]
            (play-from-hand state :runner "Cache")
            (click-card state :runner "Sahasrara"))
          "1 counter for first spend")
      (is (changed? [(get-counters (refresh twin) :power) 0]
            (play-from-hand state :runner "Cache")
            (click-card state :runner "Sahasrara"))
          "no counter for second spend")
      (take-credits state :corp)
      (core/add-counter state :runner (refresh twin) :power 3)
      (run-empty-server state :hq)
      (is (changed? [(get-counters (refresh twin) :power) -2]
            (click-prompt state :runner "2"))
          "spent 2 counters on HQ")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (run-empty-server state :rd)
      (is (changed? [(get-counters (refresh twin) :power) -2]
            (click-prompt state :runner "2"))
          "spent 2 counters on R&D")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No prompt left over"))))

(deftest theophilius-bagbiter
  ;; Theophilius Bagbiter - hand size is equal to credit pool
  (do-game
    (new-game {:runner {:deck ["Theophilius Bagbiter"]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Runner starts with 5c")
    (play-from-hand state :runner "Theophilius Bagbiter")
    (is (zero? (:credit (get-runner))) "Runner loses all credits on install")
    (is (= 1 (count (get-resource state))) "Theophilius Bagbiter installed")
    (is (zero? (hand-size :runner)) "Max hand size is 0")
    (change state :runner :credit 7)
    (is (= 7 (:credit (get-runner))) "Runner has 7c")
    (is (= 7 (hand-size :runner)) "Max hand size is 7")
    (take-credits state :runner)
    (gain-tags state :runner 1)
    (trash-resource state)
    (click-card state :corp (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))) "Theo is trashed")
    (is (empty? (get-resource state)) "No resources installed")
    (is (= 5 (hand-size :runner)) "Max hand size is reset to default")))

(deftest thunder-art-gallery-works-when-removing-avoiding-tags
    ;; Works when removing/avoiding tags
    (do-game
      (new-game {:runner {:deck ["Thunder Art Gallery" "New Angeles City Hall" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Thunder Art Gallery")
      (core/gain state :runner :credit 1)
      (gain-tags state :corp 1)
      (remove-tag state :runner)
      (click-card state :runner "New Angeles City Hall")
      (is (= 1 (:credit (get-runner))) "Runner paid one less to install (but 2 to remove tag)")
      (is (= "New Angeles City Hall" (:title (get-resource state 1))) "NACH is installed")
      (take-credits state :runner)
      (is (= 3 (:credit (get-runner))) "Runner is now at 3 credits")
      (gain-tags state :corp 1)
      (card-ability state :runner (get-resource state 1) 0)
      (click-card state :runner "Corroder")
      (is (zero? (:credit (get-runner))) "Runner paid one less to install")
      (is (= "Corroder" (:title (get-program state 0))) "Corroder is installed")))

(deftest thunder-art-gallery-interaction-with-sahasrara-and-pay-credits
    ;; Interaction with Sahasrara and pay-credits
    (do-game
      (new-game {:runner {:deck ["Thunder Art Gallery" "Sahasrara" "Darwin"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Thunder Art Gallery")
      (play-from-hand state :runner "Sahasrara")
      (core/gain state :runner :credit 4)
      (gain-tags state :corp 1)
      (let [rara (get-program state 0)]
        (is (changed? [(:credit (get-runner)) -2]
              (remove-tag state :runner)
              (click-card state :runner "Darwin")
              (click-card state :runner rara)
              (click-card state :runner rara))
            "Used TAG and Sahasrara to install Darwin for free")
        (is (= 0 (count (:hand (get-runner)))) "Installed Darwin"))))

(deftest thunder-art-gallery-correct-prompts-with-jesminder-3860
    ;; Correct prompts with Jesminder #3860
    (do-game
      (new-game {:runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :hand ["Thunder Art Gallery" "Hot Pursuit" "Datasucker"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Thunder Art Gallery")
      (play-from-hand state :runner "Hot Pursuit")
      (run-continue state)
      (click-card state :runner "Datasucker")
      (click-prompt state :runner "No action")
      (is (nil? (:run @state)) "Run has correctly ended")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (get-program state 0) :virus)) "Datasucker didn't duplicate effects")
      (is (zero? (count-tags state)) "Jesminder avoided Hot Pursuit tag")))

(deftest thunder-art-gallery-on-no-credits
    ;; Thunder Art Gallery - no install on 0 credits
    (do-game
      (new-game {:runner {:hand ["Thunder Art Gallery" "Boomerang" "Citadel Sanctuary" "Self-modifying Code"]}})
      (gain-tags state :runner 1)
      (take-credits state :corp)
      (play-from-hand state :runner "Thunder Art Gallery")
      (play-from-hand state :runner "Citadel Sanctuary")
      (play-from-hand state :runner "Self-modifying Code")
      (click-credit state :runner)
      (click-credit state :runner)
      (end-turn state :runner)
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      (is (= 1 (count (:choices (prompt-map :runner)))) "Only one choice in prompt")
      (is (= :waiting (prompt-type :corp)))
      (click-prompt state :runner "Done")
      (is (no-prompt? state :corp))))

(deftest tri-maf-contact
  ;; Tri-maf Contact - Click for 2c once per turn; take 3 meat dmg when trashed
  (do-game
    (new-game {:runner {:deck ["Tri-maf Contact" (qty "Cache" 3) "Shiv"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Tri-maf Contact")
    (let [tmc (get-resource state 0)]
      (card-ability state :runner tmc 0)
      (is (= 5 (:credit (get-runner))) "Gained 2c")
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (card-ability state :runner tmc 0)
      (is (= 5 (:credit (get-runner))) "No credits gained; already used this turn")
      (core/move state :runner tmc :hand)
      (is (= 5 (count (:hand (get-runner)))) "No meat damage")
      (play-from-hand state :runner "Tri-maf Contact")
      (gain-tags state :runner 1)
      (take-credits state :runner)
      (trash-resource state)
      (click-card state :corp (get-resource state 0))
      (is (= 4 (count (:discard (get-runner)))) "Took 3 meat damage"))))

(deftest trickster-taka
  ;; Trickster Taka - Companion, credits spendable on programs during runs (not during access)
  (do-game
      (new-game {:corp {:hand ["Hostile Takeover" "PAD Campaign"]}
                 :runner {:hand ["Trickster Taka"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Trickster Taka")
      (let [tt (get-resource state 0)]
        (core/add-counter state :runner (refresh tt) :credit 4)
        (is (= 4 (get-counters (refresh tt) :credit)) "Taka counters added")
        (let [credits (:credit (get-runner))
              counters (get-counters (refresh tt) :credit)]
          (run-on state "Server 1")
          (card-ability state :runner tt 0)
          (is (= (dec counters) (get-counters (refresh tt) :credit)) "Spent 1c from Taka during a run")
          (is (= (inc credits) (:credit (get-runner)))))
        (let [tags (count-tags state)
              credits (:credit (get-runner))
              counters (get-counters (refresh tt) :credit)]
          (run-continue state)
          (card-ability state :runner tt 0)
          (is (= counters (get-counters (refresh tt) :credit)) "Can't spend credits on Taka once run is successful")
          (is (= credits (:credit (get-runner))))
          (click-prompt state :runner "No action")
          (take-credits state :runner)
          (click-prompt state :runner "Take 1 tag")
          (is (= (inc tags) (count-tags state)) "Took 1 tag to not trash Taka")
          (is (refresh tt) "Taka not trashed")
          (is (not (find-card "Trickster Taka" (:discard (get-runner)))) "Taka not in discard yet"))
        (take-credits state :corp)
        (take-credits state :runner)
        (let [tags (count-tags state)]
          (click-prompt state :runner "Trash Trickster Taka")
          (is (= tags (count-tags state)) "Didn't pay to trash Taka")
          (is (nil? (refresh tt)) "Taka not installed")
          (is (find-card "Trickster Taka" (:discard (get-runner))) "Taka trashed")))))

(deftest trickster-taka-triggers-net-mercur-issue-4081
    ;; Triggers Net Mercur. Issue #4081
    (do-game
      (new-game {:corp {:hand ["Hostile Takeover" "PAD Campaign"]}
                 :runner {:hand ["Trickster Taka" "Net Mercur"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Net Mercur")
      (play-from-hand state :runner "Trickster Taka")
      (let [tt (get-resource state 0)]
        (core/add-counter state :runner (refresh tt) :credit 4)
        (is (= 4 (get-counters (refresh tt) :credit)) "Taka counters added"))
      (let [tt (get-resource state 0)]
        (run-on state "Server 1")
        (run-continue state)
        (card-ability state :runner tt 0)
        (is (= "Choose one" (-> (prompt-map :runner) :msg))
            "Net Mercur fires as Taka credits are stealth")
        (click-prompt state :runner "Place 1 [Credits] on Net Mercur"))))

(deftest trickster-taka-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:hand ["Trickster Taka" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Refractor")
      (play-from-hand state :runner "Trickster Taka")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [refr (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (run-on state :hq)
              (run-continue state)
              (card-ability state :runner refr 1)
              (click-card state :runner refr))
            "Used 1 credit from Trickster Taka"))))

(deftest tsakhia-bankhar-gantula
  ;; Tsakhia "Bankhar" Gantulga
  (do-game
    (new-game {:runner {:hand ["Tsakhia \"Bankhar\" Gantulga" (qty "Sure Gamble" 4)]}
               :corp {:hand ["Akhet"]}})
    (play-from-hand state :corp "Akhet" "HQ")
    (let [ak (get-ice state :hq 0)]
      (rez state :corp (refresh ak))
      (take-credits state :corp)
      (play-from-hand state :runner "Tsakhia \"Bankhar\" Gantulga")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "HQ")
      (run-on state :hq)
      (run-continue state)
      (is (changed? [(count (:hand (get-runner))) -2
             (:credit (get-corp)) 0]
            (fire-subs state (refresh ak)))
          "took 2 net damage")
      (run-continue state))))

(deftest urban-art-vernissage
  (do-game
      (new-game {:runner {:hand ["Urban Art Vernissage" "Botulus" "Monkeywrench"]
                          :credits 10}
                 :corp {:hand [(qty "Ice Wall" 2)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Urban Art Vernissage")
      (let [iw1 (get-ice state :hq 0)
            iw2 (get-ice state :rd 0)
            uav (get-resource state 0)]
        (play-from-hand state :runner "Monkeywrench")
        (click-card state :runner iw1)
        (play-from-hand state :runner "Botulus")
        (click-card state :runner iw2)
        (take-credits state :runner)
        (take-credits state :corp)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (card-ability state :runner uav 0)
        (is (changed? [(get-counters (refresh uav) :credit) 0]
              (click-card state :runner (first (:hosted (refresh iw2)))))
            "Can't choose virus trojans")
        (is (changed? [(get-counters (refresh uav) :credit) 2]
              (click-card state :runner (first (:hosted (refresh iw1)))))
            "2 credits placed on Urban Art Vernissage")
        (is (= 1 (count (:hand (get-runner)))) "1 card added to the grip")
        (play-from-hand state :runner "Monkeywrench")
        (dotimes [_ 2]
          (click-card state :runner uav)))))

(deftest valentina-ferreira-carvalho
  (do-game
    (new-game {:runner {:hand ["Valentina Ferreira Carvalho" "No Free Lunch"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Valentina Ferreira Carvalho")
    (play-from-hand state :runner "No Free Lunch")
    (gain-tags state :runner 1)
    (is (changed? [(:credit (get-runner)) 1]
                  (card-ability state :runner (get-resource state 1) 1))
        "Runner gains a credit for removing a tag")))

(deftest valentina-ferreira-carvalho-threat
  (do-game
    (new-game {:corp {:hand ["Obokata Protocol"]}
               :runner {:hand [(qty "Valentina Ferreira Carvalho" 2)]}})
    (play-and-score state "Obokata Protocol")
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Valentina Ferreira Carvalho")
    (is (changed? [(count-tags state) -1]
                  (click-prompt state :runner "Remove 1 tag"))
        "Threat 3: Runner can remove a tag when installing Valentina")
    (trash state :runner (get-resource state 0))
    (play-from-hand state :runner "Valentina Ferreira Carvalho")
    (is (changed? [(:credit (get-runner)) 2]
                  (click-prompt state :runner "Gain 2 [Credits]"))
        "Threat 3: Runner can gain 2 credits when installing Valentina")))

(deftest valentina-ferreira-carvalho-op-tag
  (do-game
    (new-game {:runner {:hand ["Valentina Ferreira Carvalho"]}
               :corp {:hand ["End of the Line"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Valentina Ferreira Carvalho")
    (gain-tags state :runner 1)
    (take-credits state :runner)
    (is (changed? [(:credit (get-runner)) 0]
                  (play-from-hand state :corp "End of the Line"))
        "Runner does NOT gain a credit from EOTL")))

(deftest verbal-plasticity
  ;; Verbal Plasticity
  (do-game
      (new-game {:runner {:hand ["Verbal Plasticity"]
                          :deck [(qty "Sure Gamble" 8)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Verbal Plasticity")
      (is (changed? [(count (:hand (get-runner))) 2]
            (click-draw state :runner))
          "First draw action draws 2 cards")
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-draw state :runner))
          "Second draw action draws only 1 card")))

(deftest verbal-plasticity-the-first-and-second-time-you-draw-card-each-turn-with-gcs-installed-draw-one-card
    ;; The first and second time you draw card each turn (with GCS installed), draw one card
    (do-game
      (new-game {:runner {:hand ["Verbal Plasticity" "Gene Conditioning Shoppe"]
                          :deck [(qty "Sure Gamble" 8)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Verbal Plasticity")
      (play-from-hand state :runner "Gene Conditioning Shoppe")
      (is (changed? [(count (:hand (get-runner))) 2]
            (click-draw state :runner))
          "First draw action draws 2 cards")
      (is (changed? [(count (:hand (get-runner))) 2]
            (click-draw state :runner))
          "Second draw action draws also 2 cards")))

(deftest virus-breeding-ground
  ;; Virus Breeding Ground - Gain counters
  (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (let [vbg (get-resource state 0)]
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 2 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn"))))

(deftest virus-breeding-ground-can-move-to-programs-pumped-by-hivemind
    ;; Can move to programs pumped by Hivemind
    (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground" "Hivemind" "Aumakua"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Hivemind")
      (play-from-hand state :runner "Aumakua")
      (let [aum (get-program state 1)
            vbg (get-resource state 0)]
        (is (zero? (get-counters aum :virus)) "Aumakua starts with 0 counters (excluding Hivemind)")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner vbg 0)
        (click-card state :runner aum)
        (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))

(deftest virus-breeding-ground-move-counters
    ;; Move counters
    (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Hivemind")
      (let [hive (get-program state 0)
            vbg (get-resource state 0)]
        (is (= 1 (get-counters hive :virus)) "Hivemind starts with 1 counter")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner vbg 0)
        (click-card state :runner hive)
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))

(deftest virus-breeding-ground-move-counters-to-a-non-virus-resource
    ;; Move counters to a non-virus resource
    (do-game
      (new-game {:runner {:deck ["Virus Breeding Ground" "Crypt"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Crypt")
      (let [vbg (get-resource state 0)
            crypt (get-resource state 1)]
        (is (zero? (get-counters crypt :virus)) "Crypt starts with 0 counters")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :runner (refresh vbg) 0)
        (click-card state :runner (refresh crypt))
        (click-prompt state :runner "Done")
        (is (zero? (get-counters (refresh crypt) :virus)) "Crypt doesn't gain a counter")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground doesn't lose a counter")
        (run-empty-server state "Archives")
        (click-prompt state :runner "Yes")
        (is (= 1 (get-counters (refresh crypt) :virus)) "Crypt gained a counter")
        (card-ability state :runner (refresh vbg) 0)
        (click-card state :runner (refresh crypt))
        (is (= 2 (get-counters (refresh crypt) :virus)) "Crypt gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))

(deftest wasteland
  ;; Wasteland - Gain 1c the first time you trash an installed card of yours each turn
  (do-game
    (new-game {:corp {:deck ["PAD Campaign"]}
               :runner {:deck ["Wasteland" "Faust" (qty "Fall Guy" 4)]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (core/gain state :runner :credit 4)
    (draw state :runner)
    (play-from-hand state :runner "Faust")
    (play-from-hand state :runner "Wasteland")
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 4 [Credits] to trash") ; Trash PAD campaign
    (is (zero? (:credit (get-runner))) "Gained nothing from Wasteland on corp trash")
    ; trash from hand first which should not trigger #2291
    (let [faust (get-program state 0)]
      (card-ability state :runner faust 1)
      (click-card state :runner (first (:hand (get-runner))))) ;discards a card
    (is (zero? (:credit (get-runner))) "Gained nothing from Wasteland")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Fall Guy")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 2 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 3 (:credit (get-runner))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (take-credits state :runner)
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 3 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 6 (:credit (get-runner))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (card-ability state :runner (get-resource state 1) 1)
    (is (= 4 (count (:discard (get-runner)))) "Fall Guy trashed")
    (is (= 8 (:credit (get-runner))) "Gained 2c from Fall Guy but no credits from Wasteland")))

(deftest whistleblower-agenda-interactions
  (doseq [agenda-name ["The Future Perfect" "NAPD Contract" "Degree Mill" "Ikawah Project" "Obokata Protocol"]]
    (testing (str "Whistleblower " agenda-name " interaction")
      (do-game
       (new-game {:corp {:deck [(qty agenda-name 2)]}
                  :runner {:deck ["Whistleblower"]}})
       (starting-hand state :corp [agenda-name agenda-name])
       (take-credits state :corp)
       (play-from-hand state :runner "Whistleblower")
       (run-empty-server state "HQ")
       (click-prompt state :runner "Yes")
       (click-prompt state :runner agenda-name)
       (is (find-card "Whistleblower" (:discard (get-runner))) "Whistleblower trashed")
       (is (no-prompt? state :runner))
       (is (not (:run @state)) "Run ended")
       (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner score area")
       (take-credits state :runner)
       (take-credits state :corp)
       (run-empty-server state "HQ")
       (is (not (no-prompt? state :runner)) "Whistleblower does not persist between runs, so agenda not autostolen")
       (is (:run @state) "Run not ended yet")
       (is (= 1 (count (:scored (get-runner)))) "Agenda not added to runner score area yet")))))

(deftest whistleblower-fetal-ai-aoycr-interaction
    ;; Fetal AI + AOYCR interaction
    (do-game
      (new-game {:corp {:deck [(qty "Fetal AI" 10) "An Offer You Can't Refuse"]}
                 :runner {:deck [(qty "Whistleblower" 5)]}})
      (starting-hand state :corp ["An Offer You Can't Refuse"])
      (take-credits state :corp)
      (play-from-hand state :runner "Whistleblower")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Fetal AI")
      (is (find-card "Whistleblower" (:discard (get-runner))) "Whistleblower trashed")
      (is (no-prompt? state :runner))
      (is (not (:run @state)) "Run ended")
      (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner score area")
      (is (= 4 (count (:hand (get-runner)))) "No damage dealt")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (is (= 2 (count (:hand (get-runner)))) "Whistleblower does not persist between turns, so Fetal AI fires")
      (click-prompt state :runner "Pay to steal")
      (play-from-hand state :runner "Whistleblower")
      (core/move state :runner (find-card "Whistleblower" (:discard (get-runner))) :hand)
      (is (= 2 (count (:hand (get-runner)))) "2 cards in hand")
      (take-credits state :runner)
      (is (= 2 (count (:scored (get-runner)))) "Agenda added to runner score area")
      (play-from-hand state :corp "An Offer You Can't Refuse")
      (click-prompt state :corp "R&D")
      (click-prompt state :runner "Yes")
      (run-continue state)
      (click-prompt state :runner "Yes") ; trigger Whistleblower
      (click-prompt state :runner "Fetal AI")
      (is (= 0 (count (:hand (get-runner)))) "Fetal AI deals net before Whistleblower triggers on Corp turn")
      (is (no-prompt? state :runner))
      (is (not (:run @state)) "Run ended")
      (is (= 3 (count (:scored (get-runner)))) "Agenda added to runner score area without needing to pay")))

(deftest whistleblower-autoresolve-functionality
    ;; Autoresolve functionality
    (do-game
     (new-game {:corp {:deck ["NAPD Contract"]}
                :runner {:deck ["Whistleblower"]}})
     (play-from-hand state :corp "NAPD Contract" "New remote")
     (take-credits state :corp)
     (play-from-hand state :runner "Whistleblower")
     (card-ability state :runner (get-resource state 0) 0)
     (click-prompt state :runner "Never") ; auto-refuse to trash whistleblower
     (run-empty-server state "Server 1")
     (click-prompt state :runner "No action")
     (is (= 0 (count (:scored (get-runner)))) "Runner could not steal NAPD Contract")
     (card-ability state :runner (get-resource state 0) 0)
     (click-prompt state :runner "Always") ; auto-trigger whistleblower
     (run-empty-server state "Server 1")
     (click-prompt state :runner "NAPD Contract")
     (is (no-prompt? state :runner))
     (is (not (:run @state)) "Run ended")
     (is (= 1 (count (:scored (get-runner)))) "Stole agenda")))

(deftest whistleblower-steal-triggers-happen-on-whistleblowing
    ;; Steal triggers happen on Whistleblowing
    (do-game
     (new-game {:corp {:deck [(qty "Project Beale" 3)]}
                :runner {:id "Leela Patel: Trained Pragmatist"
                         :deck ["Whistleblower"]}})
     (dotimes [_ 3]
       (play-from-hand state :corp "Project Beale" "New remote"))
     (take-credits state :corp)
     (play-from-hand state :runner "Whistleblower")
     (run-empty-server state "Server 1")
     (click-prompt state :runner "Yes")
     (click-prompt state :runner "Project Beale")
     (is (not (no-prompt? state :runner)) "There is an open prompt, as Leela triggers")
     (is (= "Leela Patel: Trained Pragmatist" (:title (:card (prompt-map :runner))))
         "Leela triggers, as Whistleblower does not eat steal trigger")))

(deftest xanadu
  ;; Xanadu - Increase all ice rez cost by 1 credit
  (do-game
    (new-game {:corp {:deck [(qty "Paper Wall" 2) "Launch Campaign"]}
               :runner {:deck ["Xanadu"]}})
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Xanadu")
    (let [pw1 (get-ice state :hq 0)
          pw2 (get-ice state :rd 0)
          lc (get-content state :remote1 0)]
      (rez state :corp pw1)
      (is (= 4 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (rez state :corp pw2)
      (is (= 3 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (rez state :corp lc)
      (is (= 2 (:credit (get-corp))) "Paid 1 to rez Launch Campaign; no effect on non-ice"))))

(deftest zona-sul-shipping-trash-on-install
  ;; Zona Sul Shipping - Gain 1c per turn, click to take all credits. Trash when tagged
  (do-game
    (new-game {:runner {:deck ["Zona Sul Shipping"]}})
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Zona Sul Shipping")
    (is (= 1 (count (:discard (get-runner)))) "Zona Sul trashed when tag taken")))

(deftest zona-sul-shipping
  ;; Zona Sul Shipping - Gain 1c per turn, click to take all credits. Trash when tagged
  (do-game
    (new-game {:runner {:deck ["Zona Sul Shipping"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Zona Sul Shipping")
    (let [zss (get-resource state 0)]
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (get-counters (refresh zss) :credit)) "Zona Sul holds 1c")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh zss) :credit)) "Zona Sul holds 2c")
      (card-ability state :runner zss 0)
      (is (= 12 (:credit (get-runner))) "Took 2c off Zona Sul")
      (is (= 3 (:click (get-runner))) "Spent 1 click")
      (gain-tags state :runner 1)
      (is (= 1 (count (:discard (get-runner)))) "Zona Sul trashed when tag taken"))))
