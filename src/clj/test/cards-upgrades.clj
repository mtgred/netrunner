(in-ns 'test.core)

(deftest amazon-industrial-zone
  "Amazon Industrial Zone - Immediately rez ICE installed over its server at 3 credit discount"
  (do-game
    (new-game (default-corp [(qty "Spiderweb" 1) (qty "Amazon Industrial Zone" 1)])
              (default-runner))
    (take-credits state :corp 1)
    (play-from-hand state :corp "Amazon Industrial Zone" "New remote")
    (let [aiz (get-content state :remote1 0)]
      (core/rez state :corp aiz)
      (is (= 2 (:credit (get-corp))))
      (play-from-hand state :corp "Spiderweb" "Server 1")
      (prompt-choice :corp "Yes") ; optional ability
      (let [spid (get-ice state :remote1 0)]
        (is (get-in (refresh spid) [:rezzed]) "Spiderweb rezzed")
        (is (= 1 (:credit (get-corp))) "Paid only 1 credit to rez")))))

(deftest chilo-city-grid
  "ChiLo City Grid - Give 1 tag for successful traces during runs on its server"
  (do-game
    (new-game (default-corp [(qty "Caduceus" 2) (qty "ChiLo City Grid" 1)])
              (default-runner))
    (play-from-hand state :corp "ChiLo City Grid" "New remote")
    (play-from-hand state :corp "Caduceus" "Server 1")
    (take-credits state :corp)
    (let [chilo (get-content state :remote1 0)
          cad (get-ice state :remote1 0)]
      (run-on state "R&D")
      (core/rez state :corp cad)
      (core/rez state :corp chilo)
      (card-ability state :corp cad 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 3 (:credit (get-corp))) "Trace was successful")
      (is (= 0 (:tag (get-runner))) "No tags given for run on different server")
      (run-successful state)
      (run-on state "Server 1")
      (card-ability state :corp cad 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 6 (:credit (get-corp))) "Trace was successful")
      (is (= 1 (:tag (get-runner)))
          "Runner took 1 tag given from successful trace during run on ChiLo server"))))

(deftest corporate-troubleshooter
  "Corporate Troubleshooter - Pay X credits and trash to add X strength to a piece of rezzed ICE"
  (do-game
    (new-game (default-corp [(qty "Quandary" 2) (qty "Corporate Troubleshooter" 1)])
              (default-runner))
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Corporate Troubleshooter" "HQ")
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Quandary" "HQ")
    (let [ct (get-content state :hq 0)
          q1 (get-ice state :hq 0)
          q2 (get-ice state :hq 1)]
      (core/rez state :corp q1)
      (is (= 8 (:credit (get-corp))))
      (core/rez state :corp ct)
      (card-ability state :corp ct 0)
      (prompt-choice :corp 5)
      (prompt-select :corp q2)
      (is (nil? (:current-strength (refresh q2))) "Outer Quandary unrezzed; can't be targeted")
      (prompt-select :corp q1)
      (is (= 5 (:current-strength (refresh q1))) "Inner Quandary boosted to 5 strength")
      (is (empty? (get-content state :hq))
          "Corporate Troubleshooter trashed from root of HQ")
      (take-credits state :corp)
      (is (= 0 (:current-strength (refresh q1)))
          "Inner Quandary back to default 0 strength after turn ends"))))

(deftest cyberdex-virus-suite-purge
  "Cyberdex Virus Suite - Purge ability"
  (do-game
    (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
              (default-runner [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
    (take-credits state :corp 2)
    ;; runner's turn
    ;; install cache and medium
    (play-from-hand state :runner "Cache")
    (let [virus-counters (fn [card] (core/get-virus-counters state :runner (refresh card)))
          cache (find-card "Cache" (get-in @state [:runner :rig :program]))
          cvs (get-content state :hq 0)] 
      (is (= 3 (virus-counters cache)))
      (play-from-hand state :runner "Medium")
      (take-credits state :runner 2)
      (core/rez state :corp cvs)
      (card-ability state :corp cvs 0)
      ;; nothing in hq content
      (is (empty? (get-content state :hq)) "CVS was trashed")
      ;; purged counters
      (is (zero? (virus-counters cache))
          "Cache has no counters")
      (is (zero? (virus-counters (find-card "Medium" (get-in @state [:runner :rig :program]))))
          "Medium has no counters"))))

(deftest cyberdex-virus-suite-access
  "Cyberdex Virus Suite - Purge on access"
  (do-game
    (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
              (default-runner [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :corp "Cyberdex Virus Suite" "New remote")
    (take-credits state :corp 2)
    ;; runner's turn
    ;; install cache and medium
    (play-from-hand state :runner "Cache")
    (let [virus-counters (fn [card] (core/get-virus-counters state :runner (refresh card)))
          cache (find-card "Cache" (get-in @state [:runner :rig :program]))
          cvs (get-content state :remote1 0)]
      (is (= 3 (virus-counters cache)))
      (play-from-hand state :runner "Medium")
      (run-empty-server state "Server 1")
      ;; corp now has optional prompt to trigger virus purge
      (prompt-choice :corp "Yes")
      ;; runner has prompt to trash CVS
      (prompt-choice :runner "Yes")
      ;; purged counters
      (is (zero? (virus-counters cache))
          "Cache has no counters")
      (is (zero? (virus-counters (find-card "Medium" (get-in @state [:runner :rig :program]))))
          "Medium has no counters"))))

(deftest ghost-branch-dedicated-response-team
  "Ghost Branch - with Dedicated Response Team"
  (do-game
    (new-game (default-corp [(qty "Ghost Branch" 1) (qty "Dedicated Response Team" 1)])
              (default-runner))
    (play-from-hand state :corp "Ghost Branch" "New remote")
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (core/gain state :corp :click 1)
    (let [gb (get-content state :remote1 0)
          drt (get-content state :remote2 0)]
      (core/advance state :corp {:card gb})
      (core/advance state :corp {:card (refresh gb)})
      (is (= 2 (:advance-counter (refresh gb))) "Ghost Branch advanced twice")
      (take-credits state :corp)
      (run-on state "Server 1")
      (core/rez state :corp drt)
      (run-successful state)
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Ghost Branch")
      (prompt-choice :corp "Yes")
      (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
      (prompt-choice :runner "Yes")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage"))))

(deftest marcus-batty-security-nexus
  "Marcus Batty - Simultaneous Interaction with Security Nexus"
  (do-game
    (new-game (default-corp [(qty "Marcus Batty" 1) (qty "Enigma" 1)])
              (default-runner [(qty "Security Nexus" 1)]))
    (play-from-hand state :corp "Marcus Batty" "HQ")
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 8)
    (play-from-hand state :runner "Security Nexus")
    (let [mb (get-content state :hq 0)
          en (get-ice state :hq 0)
          sn (-> @state :runner :rig :hardware first)]
      (run-on state "HQ")
      (core/rez state :corp mb)
      (core/rez state :corp en)
      (card-ability state :corp mb 0)
      (card-ability state :runner sn 0)
      ;; both prompts should be on Batty
      (is (= (:cid mb) (-> @state :corp :prompt first :card :cid))
          "Corp prompt is on Marcus Batty")
      (is (= (:cid mb) (-> @state :runner :prompt first :card :cid))
          "Runner prompt is on Marcus Batty")
      (prompt-choice :corp "0")
      (prompt-choice :runner "0")
      (is (= (:cid sn) (-> @state :corp :prompt first :card :cid))
          "Corp prompt is on Security Nexus")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner prompt is waiting for Corp"))))

(deftest old-hollywood-grid
  "Old Hollywood Grid - Ability"
  (do-game
    (new-game (default-corp [(qty "Old Hollywood Grid" 1) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Old Hollywood Grid" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)
    (let [ohg (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (run-on state "Server 1")
      (core/rez state :corp ohg)
      (run-successful state)
      ;; runner now chooses which to access.
      (prompt-select :runner hok)
      ;; prompt shows "You cannot steal"
      (prompt-choice :runner "OK")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas")
      (prompt-select :runner ohg)
      (prompt-choice :runner "No")
      (core/steal state :runner (find-card "House of Knives" (:hand (get-corp))))
      (run-empty-server state "Server 1")
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 2 (count (:scored (get-runner)))) "2 scored agendas"))))

(deftest old-hollywood-grid-central
  "Old Hollywood Grid - Central server"
  (do-game
    (new-game (default-corp [(qty "Old Hollywood Grid" 1) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Old Hollywood Grid" "HQ")
    (take-credits state :corp 2)
    (let [ohg (get-content state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp ohg)
      (run-successful state)
      ;; runner now chooses which to access.
      (prompt-choice :runner "Card from hand")
      ;; prompt shows "You cannot steal"
      (prompt-choice :runner "OK")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas"))))

(deftest port-anson-grid
  "Port Anson Grid - Prevent the Runner from jacking out until they trash a program"
  (do-game
    (new-game (default-corp [(qty "Port Anson Grid" 1) (qty "Data Raven" 1)])
              (default-runner [(qty "Faerie" 1) (qty "Technical Writer" 1)]))
    (play-from-hand state :corp "Port Anson Grid" "New remote")
    (play-from-hand state :corp "Data Raven" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Technical Writer")
    (play-from-hand state :runner "Faerie")
    (let [pag (get-content state :remote1 0)
          fae (get-in @state [:runner :rig :program 0])
          tw (get-in @state [:runner :rig :resource 0])]
      (run-on state "Server 1")
      (core/rez state :corp pag)
      (is (:cannot-jack-out (get-in @state [:run])) "Jack out disabled for Runner") ; UI button greyed out
      (core/trash state :runner tw)
      (is (:cannot-jack-out (get-in @state [:run])) "Resource trash didn't disable jack out prevention")
      (core/trash state :runner fae)
      (is (nil? (:cannot-jack-out (get-in @state [:run]))) "Jack out enabled by program trash")
      (run-on state "Server 1")
      (is (:cannot-jack-out (get-in @state [:run])) "Prevents jack out when upgrade is rezzed prior to run"))))

(deftest product-placement
  "Product Placement - Gain 2 credits when Runner accesses it"
  (do-game
    (new-game (default-corp [(qty "Product Placement" 1)])
              (default-runner))
    (play-from-hand state :corp "Product Placement" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (let [pp (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (is (= 9 (:credit (get-corp))) "Gained 2 credits from Runner accessing Product Placement")
      (prompt-choice :runner "Yes") ; Runner trashes PP
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-corp)))
          "No credits gained when Product Placement accessed in Archives"))))

(deftest red-herrings
  "Red Herrings - Ability"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-runner))
    (play-from-hand state :corp "Red Herrings" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (let [rh (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :corp rh)
      (run-empty-server state "Server 1")
      ;; runner now chooses which to access.
      (prompt-select :runner hok)
      ;; prompt should be asking for the 5cr cost
      (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
          "Prompt to pay 5cr")
      (prompt-choice :runner "No")
      (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas")
      (prompt-select :runner rh)
      (prompt-choice :runner "No")
      (run-empty-server state "Server 1")
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 0 (:credit (get-runner))) "Runner was charged 5cr")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest red-herrings-trash
  "Red Herrings - Cost increase even when trashed"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 3) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Red Herrings" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (core/gain state :runner :credit 1)
    (let [rh (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (run-empty-server state "Server 1")
      ;; runner now chooses which to access.
      (prompt-select :runner rh)
      (prompt-choice :runner "Yes") ; pay to trash
      (prompt-select :runner hok)
      ;; should now have prompt to pay 5cr for HoK
      (prompt-choice :runner "Yes")
      (is (= 0 (:credit (get-runner))) "Runner was charged 5cr")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest red-herrings-trash-from-hand
  "Red Herrings - Trashed from Hand"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-runner))
    (trash-from-hand state :corp "Red Herrings")
    (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
    (take-credits state :corp)

    (run-empty-server state "HQ")
    ;; prompt should be asking to steal HoK
    (is (= "Steal" (first (:choices (first (:prompt (get-runner))))))
        "Runner being asked to Steal")))

(deftest red-herrings-other-server
  "Red Herrings - Don't affect runs on other servers"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-runner))
    (play-from-hand state :corp "Red Herrings" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (take-credits state :corp 1)

    (let [rh (get-content state :remote1 0)]
      (core/rez state :corp rh)
      (run-empty-server state "Server 2")
      ;; access is automatic
      (prompt-choice :runner "Steal")
      (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest strongbox
  "Strongbox - Ability"
  (do-game
    (new-game (default-corp [(qty "Strongbox" 1) (qty "House of Knives" 1)])
              (default-runner))
    (play-from-hand state :corp "Strongbox" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (let [sb (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :corp sb)
      (run-empty-server state "Server 1")
      (prompt-select :runner hok)
      (is (= "House of Knives" (:title (:card (first (:prompt (get-runner))))))
          "Prompt to pay 5cr")
      (prompt-choice :runner "No")
      (is (= 3 (:click (get-runner))) "Runner was not charged 1click")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas")
      (prompt-select :runner sb)
      (prompt-choice :runner "No")
      (run-empty-server state "Server 1")
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 1 (:click (get-runner))) "Runner was charged 1click")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest strongbox-trash
  "Strongbox - Click cost even when trashed"
  (do-game
    (new-game (default-corp [(qty "Strongbox" 3) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Strongbox" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (core/gain state :runner :credit 1)
    (let [sb (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :corp sb)
      (run-empty-server state "Server 1")
      (prompt-select :runner sb)
      (prompt-choice :runner "Yes") ; pay to trash
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 2 (:click (get-runner))) "Runner was charged 1click")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest valley-grid-trash
  "Valley Grid - Reduce Runner max hand size and restore it even if trashed"
  (do-game
    (new-game (default-corp [(qty "Valley Grid" 3) (qty "Ice Wall" 3)])
              (default-runner))
    (play-from-hand state :corp "Valley Grid" "New remote")
    (take-credits state :corp 2)
    (run-on state "Server 1")
    (let [vg (get-content state :remote1 0)]
      (core/rez state :corp vg)
      (card-ability state :corp vg 0)
      (card-ability state :corp vg 0) ; only need the run to exist for test, just pretending the Runner has broken all subs on 2 ice
      (is (= 3 (core/hand-size state :runner)) "Runner max hand size reduced by 2")
      (is (= 2 (get-in (refresh vg) [:times-used])) "Saved number of times Valley Grid used")
      (run-successful state)
      (prompt-choice :runner "Yes") ; pay to trash
      (take-credits state :runner 3)
      (is (= 5 (core/hand-size state :runner)) "Runner max hand size increased by 2 at start of Corp turn"))))
