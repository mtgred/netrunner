(in-ns 'test.core)

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

(deftest street-peddler-ability
  "Street Peddler - Ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Street Peddler" 1) (qty "Gordian Blade" 1) (qty "Torch" 1)
                                              (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    ; move Gordian back to deck
    (core/move state :runner (find-card "Gordian Blade" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Torch" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Street Peddler")
    (let [sp (get-in @state [:runner :rig :resource 0])]
      (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
      (card-ability state :runner sp 0)
      (is (= 1 (count (:choices (first (:prompt (get-runner)))))) "Only 1 choice to install off Peddler")
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
      (is (= 1 (count (:choices (first (:prompt (get-runner)))))) "Only 1 choice to install off Peddler")
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
      (core/select state :runner {:card (refresh hive)})
      (is (= 2 (get (refresh hive) :counter 0)) "Hivemind gained 1 counter")
      (is (= 0 (get (refresh vbg) :counter 0)) "Virus Breeding Ground lost 1 counter"))))

