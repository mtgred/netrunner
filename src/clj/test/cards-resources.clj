(in-ns 'test.core)

(deftest film-critic-fetal-ai
  "Film Critic - Fetal AI interaction"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Film Critic" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Film Critic")
    (let [fc (first (get-in @state [:runner :rig :resource]))]
      (core/click-run state :runner {:server :hq})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      ; should not have taken damage yet
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt yet")
      (card-ability state :runner fc 0)
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt")
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (card-ability state :runner fc 1)
      (is (= 1 (count (:scored (get-runner)))) "Agenda added to runner scored")
      (is (= 3 (count (:hand (get-runner)))) "No damage dealt"))))


(deftest kati-jones
  "Kati Jones - Click to store and take"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Kati Jones" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (is (= 3 (:credit (get-runner))))
    (let [kati (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))))
      (is (= 3 (:counter (refresh kati))) "Store 3cr on Kati")
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))) "Second use of Kati should not be allowed")
      (is (= 3 (:counter (refresh kati))) "Second use of Kati should not be allowed")
      (take-credits state :runner 2)
      (is (= 5 (:credit (get-runner))) "Pass turn, take 2cr")
      (take-credits state :corp)
      (card-ability state :runner kati 0)
      (is (= 6 (:counter (refresh kati))) "Store 3cr more on Kati")
      (take-credits state :runner 3)
      (is (= 8 (:credit (get-runner))) "Pass turn, take 3cr")
      (take-credits state :corp)
      (card-ability state :runner (refresh kati) 1)
      (is (= 14 (:credit (get-runner))) "Take 6cr from Kati")
      (is (zero? (:counter (refresh kati))) "No counters left on Kati"))))

(deftest security-testing
  "Security Testing - Ability"
  (do-game
    (new-game (default-corp [(qty "Jackson Howard" 1)]) (default-runner [(qty "Security Testing" 1)]))
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Security Testing")
    (let [st (get-in @state [:runner :rig :resource 0])]
      (take-credits state :runner 3)
      (take-credits state :corp)
      (prompt-choice :runner "Server 1")
      (core/run state :runner :remote1)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (is (= 10 (:credit (get-runner))) "Gained 2 credits from Security Testing")
      (core/run state :runner :remote1)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-choice :runner "No")
      (is (= 10 (:credit (get-runner))) "Did not gain credits on second run")
      (take-credits state :runner 2)

      (take-credits state :corp)
      (prompt-choice :runner "Server 1")
      (core/run state :runner :archives)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (is (= 12 (:credit (get-runner))) "Did not gain credits when running other server"))))

(deftest street-peddler-ability
  "Street Peddler - Ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Street Peddler" 1) (qty "Gordian Blade" 1) (qty "Yog.0" 1)
                                              (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    ; move Gordian back to deck
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Yog.0" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
      (card-ability state :runner sp 0)
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:runner :rig :program 0]))) "Gordian Blade was installed")
      (is (= 3 (:memory (get-runner))) "Gordian cost 1 mu"))))

(deftest street-peddler-kate-discount
  "Street Peddler - Interaction with Kate discount"
  (do-game
    (new-game (default-corp) (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                                        [(qty "Street Peddler" 1) (qty "Gordian Blade" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    ; move Gordian back to deck
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (core/lose state :runner :credit 3) ; should still be able to afford Gordian w/ Kate discount
      (card-ability state :runner sp 0)
      (is (= 2 (count (:choices (first (:prompt (get-runner)))))) "Only 1 choice (plus Cancel) to install off Peddler")
      (prompt-card :runner (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:runner :rig :program 0]))) "Gordian Blade was installed")
      (is (= 3 (:memory (get-runner))) "Gordian cost 1 mu"))))

(deftest street-peddler-memory-units
  "Street Peddler - Programs Should Cost Memory. Issue #708"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Street Peddler" 1) (qty "Corroder" 3)]))
    (take-credits state :corp)
    ; move Corroders back to deck
    (core/move state :runner (find-card "Corroder" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Corroder" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Corroder" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (is (= 4 (:memory (get-runner))) "No memory cost for hosting on Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
      (card-ability state :runner sp 0)
      (prompt-card :runner (first (:hosted sp))) ; choose to install Gordian
      (is (= "Corroder" (:title (get-in @state [:runner :rig :program 0]))) "Corroder was installed")
      (is (= 3 (:memory (get-runner))) "Corroder cost 1 mu"))))


(deftest street-peddler-parasite-1cr
  "Street Peddler - Installing Parasite with only 1cr. Issue #491."
  (do-game
    (new-game (default-corp [(qty "Pop-up Window" 3)]) (default-runner [(qty "Street Peddler" 1) (qty "Parasite" 3)]))
    (play-from-hand state :corp "Pop-up Window" "HQ")
    (take-credits state :corp 2)
    ; move Parasites back to deck
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (core/lose state :runner :credit 4) ; go down to 1 credit
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])
          pu (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp pu)
      (card-ability state :runner sp 0)
      (prompt-card :runner (first (:hosted sp))) ; choose to install Parasite
      (is (= "Parasite" (:title (:card (first (get-in @state [:runner :prompt]))))) "Parasite target prompt")
      (prompt-select :runner pu)
      (is (= 4 (count (:discard (get-runner)))) "3 Parasite, 1 Street Peddler in heap")
      (is (= 1 (count (:discard (get-corp)))) "Pop-up Window in archives"))))

(deftest the-supplier-ability
  "The Supplier - Ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "The Supplier" 1) (qty "Plascrete Carapace" 1) (qty "Utopia Shard" 1)
                                              (qty "Hedge Fund" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Supplier")
    (let [ts (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner ts 0)
      (prompt-select :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
      (card-ability state :runner ts 0)
      (is (= 1 (count (get-in @state [:runner :prompt 0 :choices]))))
      (prompt-select :runner (find-card "Utopia Shard" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh ts)))) "The Supplier is hosting 2 cards")
      (core/end-turn state :runner nil)
      (take-credits state :corp)
      ; Utopia Shard cannot be afforded and should not be in the prompt
      (is (= 2 (count (get-in @state [:runner :prompt 0 :choices]))) "1 card and 'No install' choices for The Supplier")
      (prompt-card :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 1 (:credit (get-runner))) "Runner charged 1 credit to install Plascrete off The Supplier")
      (take-credits state :runner)
      (is (= 5 (:credit (get-runner))))
      (take-credits state :corp)
      (prompt-choice :runner "No install")
      (is (= 0 (count (get-in @state [:runner :prompt]))) "Resolved The Supplier prompt")
      (is (= 1 (count (:hosted (refresh ts)))) "One card still on The Supplier"))))

(deftest the-supplier-kate-discount
  "The Supplier - Interaction with Kate discount. Issue #578."
  (do-game
    (new-game (default-corp) (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                                        [(qty "The Supplier" 1) (qty "Plascrete Carapace" 1) (qty "Kati Jones" 1)
                                         (qty "Hedge Fund" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "The Supplier")
    (let [ts (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner ts 0)
      (prompt-select :runner (find-card "Plascrete Carapace" (:hand (get-runner))))
      (core/lose state :runner :credit (:credit (get-runner)))
      (core/end-turn state :runner nil)
      (take-credits state :corp)
      (is (= 2 (count (get-in @state [:runner :prompt 0 :choices]))) "Able to install Plascrete from The Supplier")
      (prompt-card :runner (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 0 (:credit (get-runner))) "Kate discount applied")
      (is (= 1 (count (get-in @state [:runner :rig :resource]))) "Plascrete installed"))))

(deftest virus-breeding-ground-gain
  "Virus Breeding Ground - Gain counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Virus Breeding Ground" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (let [vbg (get-in @state [:runner :rig :resource 0])]
      (is (zero? (get vbg :counter 0)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 1 (get (refresh vbg) :counter 0)) "Virus Breeding Ground gains 1 counter per turn")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 2 (get (refresh vbg) :counter 0)) "Virus Breeding Ground gains 1 counter per turn"))))

(deftest virus-breeding-ground-gain
  "Virus Breeding Ground - Move counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Hivemind")
    (let [hive (get-in @state [:runner :rig :program 0])
          vbg (get-in @state [:runner :rig :resource 0])]
      (is (= 1 (get hive :counter 0)) "Hivemind starts with 1 counter")
      (is (zero? (get vbg :counter 0)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 1 (get (refresh vbg) :counter 0)) "Virus Breeding Ground gains 1 counter per turn")
      (card-ability state :runner vbg 0)
      (prompt-select :runner hive)
      (is (= 2 (get (refresh hive) :counter 0)) "Hivemind gained 1 counter")
      (is (= 0 (get (refresh vbg) :counter 0)) "Virus Breeding Ground lost 1 counter"))))

(deftest ddos
  "Prevent rezzing of outermost ice for the rest of the turn"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3)]) (default-runner [(qty "DDoS" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "DDoS")
    (let [ddos (get-in @state [:runner :rig :resource 0])
          iwall (get-in @state [:corp :servers :hq :ices 0])]
      (card-ability state :runner ddos 0)
      (is (= (:title ddos) (get-in @state [:runner :discard 0 :title]) ))
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (core/end-run state :runner)
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (core/end-run state :runner)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp iwall)
      (is (get-in (refresh iwall) [:rezzed]))
      )))

(deftest daily-casts
  "Play and tick through all turns of daily casts"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Daily Casts" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Daily Casts")
    (let [dc (get-in @state [:runner :rig :resource 0])]
      ;Number of credits
      (is (= 8 (get-in dc [:counter])))
      (is (= 2 (get-in @state [:runner :credit])))
      ;End turn
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 6 (get-in (refresh dc) [:counter])))
      (is (= 7 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 4 (get-in (refresh dc) [:counter])))
      (is (= 13 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-in (refresh dc) [:counter])))
      (is (= 19 (get-in @state [:runner :credit])))
      (take-credits state :runner)
      (take-credits state :corp)
      (is (nil? (get-in @state [:runner :rig :resource 0])))
      )))
