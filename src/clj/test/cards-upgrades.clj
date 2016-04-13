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

(deftest breaker-bay-grid
  "Breaker Bay Grid - Reduce rez cost of other cards in this server by 5 credits"
  (do-game
   (new-game (default-corp [(qty "Breaker Bay Grid" 2) (qty "The Root" 1) (qty "Strongbox" 1)])
             (default-runner))
   (core/gain state :corp :click 1)
   (play-from-hand state :corp "Breaker Bay Grid" "New remote")
   (play-from-hand state :corp "The Root" "Server 1")
   (let [bbg1 (get-content state :remote1 0)
         root (get-content state :remote1 1)]
     (core/rez state :corp bbg1)
     (core/rez state :corp root)
     (is (= 4 (:credit (get-corp))) "Paid only 1 to rez The Root")
     (play-from-hand state :corp "Breaker Bay Grid" "R&D")
     (play-from-hand state :corp "Strongbox" "R&D")
     (let [bbg2 (get-content state :rd 0)
           sbox (get-content state :rd 1)]
       (core/rez state :corp bbg2)
       (core/rez state :corp sbox)
       (is (= 1 (:credit (get-corp))) "Paid full 3 credits to rez Strongbox")))))

(deftest caprice-nisei
  "Caprice Nisei - Psi game for ETR after runner passes last ice"
  (do-game
   (new-game (default-corp [(qty "Caprice Nisei" 3) (qty "Quandary" 3)])
             (default-runner))
   (play-from-hand state :corp "Caprice Nisei" "New remote")
   (take-credits state :corp)
   (let [caprice (get-content state :remote1 0)]
     ;; Check Caprice triggers properly on no ice (and rezzed)
     (core/rez state :corp caprice)
     (run-on state "Server 1")
     (is (prompt-is-card? :corp caprice)
         "Caprice prompt even with no ice, once runner makes run")
     (is (prompt-is-card? :runner caprice) "Runner has Caprice prompt")
     (prompt-choice :corp "0 [Credits]")
     (prompt-choice :runner "1 [Credits]")
     (take-credits state :runner)


     (play-from-hand state :corp "Quandary" "Server 1")
     (play-from-hand state :corp "Quandary" "Server 1")
     (take-credits state :corp)

     ;; Check Caprice triggers properly on multiple ice
     (run-on state "Server 1")
     (run-continue state)
     (is (empty? (get-in @state [:corp :prompt])) "Caprice not trigger on first ice")
     (run-continue state) ; Caprice prompt after this
     (is (prompt-is-card? :corp caprice)
         "Corp has Caprice prompt (triggered automatically as runner passed last ice)")
     (is (prompt-is-card? :runner caprice) "Runner has Caprice prompt")
     (prompt-choice :corp "0 [Credits]")
     (prompt-choice :runner "1 [Credits]")
     (is (not (:run @state)) "Run ended by Caprice")
     (is (empty? (get-in @state [:corp :prompt])) "Caprice prompted cleared")

     ;; Check Caprice does not trigger on other servers
     (run-on state "HQ")
     (is (empty? (get-in @state [:corp :prompt])) "Caprice does not trigger on other servers"))))

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
      (is (prompt-is-type? :runner :waiting) "Runner has prompt to wait for Ghost Branch")
      (prompt-choice :corp "Yes")
      (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
      (prompt-choice :runner "Yes")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage"))))

(deftest hokusai-grid
  "Hokusai Grid - Do 1 net damage when run successful on its server"
  (do-game
    (new-game (default-corp [(qty "Hokusai Grid" 1)])
              (default-runner))
    (play-from-hand state :corp "Hokusai Grid" "HQ")
    (take-credits state :corp)
    (core/rez state :corp (get-content state :hq 0))
    (run-empty-server state :rd)
    (is (empty? (:discard (get-runner))) "No net damage done for successful run on R&D")
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-runner)))) "1 net damage done for successful run on HQ")))

(deftest keegan-lane
  "Keegan Lane - Trash self and remove 1 Runner tag to trash a program"
  (do-game
    (new-game (default-corp [(qty "Keegan Lane" 1)])
              (default-runner [(qty "Corroder" 1)]))
    (play-from-hand state :corp "Keegan Lane" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (run-on state :hq)
    (let [keeg (get-content state :hq 0)]
      (core/rez state :corp keeg)
      (card-ability state :corp keeg 0)
      (is (= 1 (count (get-content state :hq))) "Keegan didn't fire, Runner has no tags")
      (core/gain state :runner :tag 2)
      (card-ability state :corp keeg 0)
      (prompt-select :corp (get-program state 0))
      (is (= 1 (:tag (get-runner))) "1 tag removed")
      (is (= 1 (count (:discard (get-corp)))) "Keegan trashed")
      (is (= 1 (count (:discard (get-runner)))) "Corroder trashed"))))

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
      (is (prompt-is-card? :corp mb) "Corp prompt is on Marcus Batty")
      (is (prompt-is-card? :runner mb) "Runner prompt is on Marcus Batty")
      (prompt-choice :corp "0")
      (prompt-choice :runner "0")
      (is (prompt-is-card? :corp sn) "Corp prompt is on Security Nexus")
      (is (prompt-is-type? :runner :waiting) "Runner prompt is waiting for Corp"))))

(deftest neotokyo-grid
  "NeoTokyo Grid - Gain 1c the first time per turn a card in this server gets an advancement"
  (do-game
    (new-game (default-corp [(qty "NeoTokyo Grid" 1) (qty "Nisei MK II" 1)
                             (qty "Shipment from SanSan" 1)])
              (default-runner))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "NeoTokyo Grid" "New remote")
    (play-from-hand state :corp "Nisei MK II" "Server 1")
    (core/rez state :corp (get-content state :remote1 0))
    (let [nis (get-content state :remote1 1)]
      (play-from-hand state :corp "Shipment from SanSan")
      (prompt-choice :corp "2")
      (prompt-select :corp nis)
      (is (= 2 (:advance-counter (refresh nis))) "2 advancements on agenda")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit")
      (core/advance state :corp {:card (refresh nis)})
      (is (= 3 (:advance-counter (refresh nis))) "3 advancements on agenda")
      (is (= 3 (:credit (get-corp))) "No credit gained"))))

(deftest off-the-grid
  "Off the Grid run ability - and interaction with RP"
  (do-game
   (new-game
    (make-deck "Jinteki: Replicating Perfection" [(qty "Off the Grid" 3)
                                                  (qty "Mental Health Clinic" 3)])
    (default-runner))
   (play-from-hand state :corp "Off the Grid" "New remote")
   (play-from-hand state :corp "Mental Health Clinic" "Server 1")
   (let [otg (get-content state :remote1 0)]
     (take-credits state :corp)
     (core/rez state :corp (refresh otg))
     (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
     (run-empty-server state "R&D")
     (is (not (core/can-run-server? state "Server 1")) "Runner cannot run on Off the Grid")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (not (core/can-run-server? state "Server 1")) "Off the Grid prevention persisted")
     (run-empty-server state "HQ")
     (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on Server 1")
     (is (= nil (refresh otg)) "Off the Grid trashed"))))

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

(deftest ryon-knight
  "Ryon Knight - Trash during run to do 1 brain damage if Runner has no clicks remaining"
  (do-game
    (new-game (default-corp [(qty "Ryon Knight" 1)])
              (default-runner))
    (play-from-hand state :corp "Ryon Knight" "HQ")
    (take-credits state :corp)
    (let [ryon (get-content state :hq 0)]
      (run-on state :hq)
      (core/rez state :corp ryon)
      (card-ability state :corp ryon 0)
      (is (= 3 (:click (get-runner))))
      (is (= 0 (:brain-damage (get-runner))))
      (is (= 1 (count (get-content state :hq))) "Ryon ability didn't fire with 3 Runner clicks left")
      (run-jack-out state)
      (take-credits state :runner 2)
      (run-on state :hq)
      (card-ability state :corp ryon 0)
      (is (= 0 (:click (get-runner))))
      (is (= 1 (:brain-damage (get-runner))) "Did 1 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "Ryon trashed"))))

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

(deftest tori-hanzo
  "Tori Hanzō - Pay to do 1 brain damage instead of net damage"
  (do-game
    (new-game (default-corp [(qty "Pup" 1) (qty "Tori Hanzō" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Net Shield" 1)]))
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Tori Hanzō" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Net Shield")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)
          tori (get-content state :hq 0)
          nshld (get-in @state [:runner :rig :program 0])]
      (core/rez state :corp pup)
      (core/rez state :corp tori)
      (card-ability state :corp pup 0)
      (card-ability state :runner nshld 0)
      (prompt-choice :runner "Done")
      (is (empty? (:discard (get-runner))) "1 net damage prevented")
      (card-ability state :corp pup 0)
      (prompt-choice :runner "Done") ; decline to prevent
      (is (= 1 (count (:discard (get-runner)))) "1 net damage; previous prevention stopped Tori ability")
      (run-jack-out state)
      (run-on state "HQ")
      (card-ability state :corp pup 0)
      (prompt-choice :runner "Done")
      (prompt-choice :corp "Yes")
      (is (= 2 (count (:discard (get-runner)))) "1 brain damage suffered")
      (is (= 1 (:brain-damage (get-runner)))))))

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
