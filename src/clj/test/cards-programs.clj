(in-ns 'test.core)

(deftest djinn-host-chakana
  "Djinn - Hosted Chakana does not disable advancing agendas. Issue #750"
  (do-game
    (new-game (default-corp [(qty "Priority Requisition" 1)])
              (default-runner [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (play-from-hand state :corp "Priority Requisition" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Djinn")
    (let [djinn (get-in @state [:runner :rig :program 0])
          agenda (get-in @state [:corp :servers :remote1 :content 0])]
      (is agenda "Agenda was installed")
      (card-ability state :runner djinn 1)
      (prompt-select :runner (find-card "Chakana" (:hand (get-runner))))
      (let [chak (first (:hosted (refresh djinn)))]
        (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
        (core/add-prop state :runner (first (:hosted (refresh djinn))) :counter 3) ; manually add 3 counters
        (take-credits state :runner 2)
        (core/advance state :corp {:card agenda})
        (is (= 1 (:advance-counter (refresh agenda))) "Agenda was advanced")))))

(deftest djinn-host-program
  "Djinn - Host a non-icebreaker program"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Djinn")
    (is (= 3 (:memory (get-runner))))
    (let [djinn (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner djinn 1)
      (prompt-select :runner (find-card "Chakana" (:hand (get-runner))))
      (is (= 3 (:memory (get-runner))) "No memory used to host on Djinn")
      (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
      (is (= 1 (:credit (get-runner))) "Full cost to host on Djinn"))))

(deftest djinn-tutor-virus
  "Djinn - Tutor a virus program"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Djinn" 1) (qty "Parasite" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Djinn")
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (is (zero? (count (:hand (get-runner)))) "No cards in hand after moving Parasite to deck")
    (let [djinn (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner djinn 0)
      (prompt-card :runner (find-card "Parasite" (:deck (get-runner))))
      (is (= "Parasite" (:title (first (:hand (get-runner))))) "Djinn moved Parasite to hand")
      (is (= 2 (:credit (get-runner))) "1cr to use Djinn ability")
      (is (= 2 (:click (get-runner))) "1click to use Djinn ability"))))

(deftest leprechaun-mu-savings
  "Leprechaun - Keep MU the same when hosting or trashing hosted programs"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Leprechaun" 1) (qty "Hyperdriver" 1) (qty "Imp" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Leprechaun")
    (let [lep (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner lep 0)
      (core/select state :runner {:card (find-card "Hyperdriver" (:hand (get-runner)))})
      (is (= 2 (:click (get-runner))))
      (is (= 2 (:credit (get-runner))))
      (is (= 3 (:memory (get-runner))) "Hyperdriver 3 MU not deducted from available MU")
      (card-ability state :runner lep 0)
      (core/select state :runner {:card (find-card "Imp" (:hand (get-runner)))})
      (is (= 1 (:click (get-runner))))
      (is (= 0 (:credit (get-runner))))
      (is (= 3 (:memory (get-runner))) "Imp 1 MU not deducted from available MU")
      (core/move state :runner (find-card "Hyperdriver" (:hosted (refresh lep))) :discard) ; trash Hyperdriver
      (is (= 3 (:memory (get-runner))) "Hyperdriver 3 MU not added to available MU")
      (core/move state :runner (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
      (is (= 3 (:memory (get-runner))) "Imp 1 MU not added to available MU"))))

(deftest magnum-opus-click
  "Magnum Opus - Gain 2 cr"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Magnum Opus" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 2 (:memory (get-runner))))
    (is (= 0 (:credit (get-runner))))
    (let [mopus (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner mopus 0)
      (is (= 2 (:credit (get-runner))) "Gain 2cr"))))

(deftest parasite-apex
  "Paraiste - Installed facedown w/ Apex"
  (do-game
    (new-game (default-corp)
              (make-deck "Apex: Invasive Predator" [(qty "Parasite" 1)]))
    (take-credits state :corp)
    (prompt-select :runner (find-card "Parasite" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "No prompt to host Parasite")
    (is (= 1 (count (get-in @state [:runner :rig :facedown]))) "Parasite installed face down")))

(deftest parasite-gain-counter
  "Parasite - Gain 1 counter every Runner turn"
  (do-game
    (new-game (default-corp [(qty "Wraparound" 3) (qty "Hedge Fund" 3)])
              (default-runner [(qty "Parasite" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Wraparound" "HQ")
    (let [wrap (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp wrap)
      (take-credits state :corp)
      (play-from-hand state :runner "Parasite")
      (prompt-select :runner wrap)
      (is (= 3 (:memory (get-runner))) "Parasite consumes 1 MU")
      (let [psite (first (:hosted (refresh wrap)))]
        (is (= 0 (:counter psite)) "Parasite has no counters yet")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (:counter (refresh psite))) "Parasite gained 1 virus counter at start of Runner turn")
        (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))

(deftest progenitor-host-hivemind
  "Progenitor - Hosting Hivemind, using Virus Breeding Ground. Issue #738"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Progenitor" 1) (qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Progenitor")
    (play-from-hand state :runner "Virus Breeding Ground")
    (is (= 4 (:memory (get-runner))))
    (let [prog (get-in @state [:runner :rig :program 0])
          vbg (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner prog 0)
      (prompt-select :runner (find-card "Hivemind" (:hand (get-runner))))
      (is (= 4 (:memory (get-runner))) "No memory used to host on Progenitor")
      (let [hive (first (:hosted (refresh prog)))]
        (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
        (is (= 1 (:counter hive)) "Hivemind has 1 counter")
        (is (= 0 (:credit (get-runner))) "Full cost to host on Progenitor")
        (take-credits state :runner 1)
        (take-credits state :corp)
        (card-ability state :runner vbg 0) ; use VBG to transfer 1 token to Hivemind
        (prompt-select :runner hive)
        (is (= 2 (get (refresh hive) :counter 0)) "Hivemind gained 1 counter")
        (is (= 0 (get (refresh vbg) :counter 0)) "Virus Breeding Ground lost 1 counter")))))
