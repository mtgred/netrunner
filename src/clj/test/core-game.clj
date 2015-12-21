(in-ns 'test.core)

(deftest runner-install-program
  "runner-install - Program; ensure costs are paid"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Gordian Blade" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (let [gord (get-in @state [:runner :rig :program 0])]
      (is (= (- 5 (:cost gord)) (:credit (get-runner))) "Program cost was applied")
      (is (= (- 4 (:memoryunits gord)) (:memory (get-runner))) "Program MU was applied"))))

(deftest desactivate-program
  "desactivate - Program; ensure MU are restored"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Gordian Blade" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Gordian Blade")
    (let [gord (get-in @state [:runner :rig :program 0])]
      (core/trash state :runner gord)
      (is (= 4 (:memory (get-runner))) "Trashing the program restored MU"))))

(deftest refresh-recurring-credits-hosted
  "host - Recurring credits on cards hosted after install refresh properly"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) (qty "Hedge Fund" 3)])
              (default-runner [(qty "Compromised Employee" 1) (qty "Off-Campus Apartment" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Off-Campus Apartment")
    (play-from-hand state :runner "Compromised Employee")
    (let [iwall (get-in @state [:corp :servers :hq :ices 0])
          apt (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner apt 1) ; use Off-Campus option to host an installed card
      (prompt-select :runner (find-card "Compromised Employee" (get-in @state [:runner :rig :resource])))
      (let [cehosted (first (:hosted (refresh apt)))]
        (card-ability state :runner cehosted 0) ; take Comp Empl credit
        (is (= 4 (:credit (get-runner))))
        (is (= 0 (:rec-counter (refresh cehosted))))
        (core/rez state :corp iwall)
        (is (= 5 (:credit (get-runner))) "Compromised Employee gave 1 credit from ice rez")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (:rec-counter (refresh cehosted))) "Compromised Employee recurring credit refreshed")))))

(deftest card-str-test-simple
  "ensure card-str names cards in simple situations properly"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) (qty "Jackson Howard" 2)])
              (default-runner [(qty "Corroder" 1) (qty "Clone Chip" 1) (qty "Paparazzi" 1) (qty "Parasite" 1)]))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/end-turn state :corp nil)
    (core/start-turn state :runner nil)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Paparazzi")
    (play-from-hand state :runner "Parasite")
    (let [hqiwall0 (get-in @state [:corp :servers :hq :ices 0])
          hqiwall1 (get-in @state [:corp :servers :hq :ices 1])
          rdiwall (get-in @state [:corp :servers :rd :ices 0])
          jh1 (first (get-in @state [:corp :servers :remote1 :content]))
          jh2 (first (get-in @state [:corp :servers :remote2 :content]))
          corr (get-in @state [:runner :rig :program 0])
          cchip (get-in @state [:runner :rig :hardware 0])
          pap (get-in @state [:runner :rig :resource 0])]
      (core/rez state :corp hqiwall0)
      (core/rez state :corp jh1)
      (prompt-select :runner (refresh hqiwall0))
      (is (= (core/card-str state (refresh hqiwall0)) "Ice Wall protecting HQ at position 0"))
      (is (= (core/card-str state (refresh hqiwall1)) "ICE protecting HQ at position 1"))
      (is (= (core/card-str state (refresh rdiwall)) "ICE protecting R&D at position 0"))
      (is (= (core/card-str state (refresh jh1)) "Jackson Howard in Server 1"))
      (is (= (core/card-str state (refresh jh2)) "a card in Server 2"))
      (is (= (core/card-str state (refresh corr)) "Corroder"))
      (is (= (core/card-str state (refresh cchip)) "Clone Chip"))
      (is (= (core/card-str state (refresh pap)) "Paparazzi"))
      (is (= (core/card-str state (first (:hosted (refresh hqiwall0)))) "Parasite hosted on Ice Wall protecting HQ at position 0"))
      )))

(deftest invalid-score-attempt
  "Test scoring with an incorrect number of advancement tokens"
  (do-game
    (new-game (default-corp [(qty "Ancestral Imager" 1)]) (default-runner))
    (play-from-hand state :corp "Ancestral Imager" "New remote")
    (let [ai (get-in @state [:corp :servers :remote1 :content 0])]
      ;Trying to score without any tokens throws NPE
      (is (thrown? java.lang.NullPointerException (core/score state :corp {:card (refresh ai)})))
      (is (not (nil? (get-in @state [:corp :servers :remote1 :content 0]))))
      (core/advance state :corp {:card (refresh ai)})
      (core/score state :corp {:card (refresh ai)})
      (is (not (nil? (get-in @state [:corp :servers :remote1 :content 0])))))))

(deftest trash-seen-and-unseen
  "Trash installed assets that are both seen and unseen by runner"
  (do-game
    (new-game (default-corp [(qty "PAD Campaign" 3)])
              (default-runner))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp 1)
    (core/click-run state :runner {:server :remote1})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "No")
    ; run and trash the second asset
    (core/click-run state :runner {:server :remote2})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Yes")
    (take-credits state :runner 2)
    (play-from-hand state :corp "PAD Campaign" "Remote 1")
    (is (= 2 (count (:discard (get-corp)))) "Trashed existing asset")
    (is (:seen (first (get-in @state [:corp :discard]))) "Asset trashed by runner is Seen")
    (is (not (:seen (second (get-in @state [:corp :discard])))) "Asset trashed by corp is Unseen")
    (is (not (:seen (first (get-in @state [:corp :servers :remote1 :content])))) "New asset is unseen")))

(deftest reinstall-seen-asset
  "Install a faceup card in Archives, make sure it is not :seen"
  (do-game
    (new-game (default-corp [(qty "PAD Campaign" 1) (qty "Interns" 1)])
              (default-runner))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp 2)
    ; run and trash the asset
    (core/click-run state :runner {:server :remote1})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Yes")
    (is (:seen (first (get-in @state [:corp :discard]))) "Asset trashed by runner is Seen")
    (take-credits state :runner 3)
    (play-from-hand state :corp "Interns")
    (prompt-select :corp (first (get-in @state [:corp :discard])))
    (prompt-choice :corp "New remote")
    (is (not (:seen (first (get-in @state [:corp :servers :remote2 :content])))) "New asset is unseen")))

(deftest counter-manipulation-commands
  "Test interactions of various cards with /counter and /adv-counter commands"
  (do-game
    (new-game (default-corp [(qty "Adonis Campaign" 1) (qty "Public Support" 2) (qty "Oaktown Renovation" 1)])
              (default-runner))
    ; Turn 1 Corp, install oaktown and assets
    (core/gain state :corp :click 4)
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (play-from-hand state :corp "Public Support" "New remote")
    (play-from-hand state :corp "Public Support" "New remote")
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (let [adonis (first (get-in @state [:corp :servers :remote1 :content]))
          publics1 (first (get-in @state [:corp :servers :remote2 :content]))
          publics2 (first (get-in @state [:corp :servers :remote3 :content]))
          oaktown (first (get-in @state [:corp :servers :remote4 :content]))]
    (core/advance state :corp {:card (refresh oaktown)})
    (core/advance state :corp {:card (refresh oaktown)})
    (core/advance state :corp {:card (refresh oaktown)})
    (is (= 8 (:credit (get-corp))) "Corp 5+3 creds from Oaktown")
    (core/end-turn state :corp nil)
    ; Turn 1 Runner
    (core/start-turn state :runner nil)
    (take-credits state :runner 3)
    (core/click-credit state :runner nil)
    (core/end-turn state :runner nil)
    (core/rez state :corp (refresh adonis))
    (core/rez state :corp (refresh publics1))
    ; Turn 2 Corp
    (core/start-turn state :corp nil)
    (core/rez state :corp (refresh publics2))
    (is (= 3 (:click (get-corp))))
    (is (= 3 (:credit (get-corp))) "only Adonis money")
    (is (= 9 (:counter (refresh adonis))))
    (is (= 2 (:counter (refresh publics1))))
    (is (= 3 (:counter (refresh publics2))))
    ; oops, forgot to rez 2nd public support before start of turn, let me fix it with a /command
    (core/command-counter state :corp 2)
    (prompt-select :corp (refresh publics2))
    (is (= 2 (:counter (refresh publics2))))
    ; Oaktown checks and manipulation
    (is (= 3 (:advance-counter (refresh oaktown))))
    (core/command-adv-counter state :corp 2)
    (prompt-select :corp (refresh oaktown))
    (core/score state :corp (refresh oaktown)) ; score should fail, shouldn't be able to score with 2 advancement tokens
    (is (= 0 (:agenda-point (get-corp))))
    (core/command-adv-counter state :corp 4)
    (prompt-select :corp (refresh oaktown))
    (is (= 4 (:advance-counter (refresh oaktown))))
    (is (= 3 (:credit (get-corp))))
    (is (= 3 (:click (get-corp))))
    (core/score state :corp (refresh oaktown)) ; now the score should go through
    (is (= 2 (:agenda-point (get-corp))))
    (take-credits state :corp)
    ; Turn 2 Runner
    (core/command-counter state :corp 1) ; cheating with publics1 going too fast. Why? because I can
    (prompt-select :corp (refresh publics1))
    (core/command-counter state :corp 3) ; let's adjust Adonis while at it
    (prompt-select :corp (refresh adonis))
    (take-credits state :runner)
    ; Turn 3 Corp
    (is (= 3 (:agenda-point (get-corp)))) ; cheated PS1 should get scored
    (is (= 9 (:credit (get-corp))) "twice Adonis money and money turn")
    (is (= (:zone (refresh publics1) :scored)))
    (is (= (:zone (refresh publics2)) [:servers :remote3 :content]))
    (is (= (:zone (refresh adonis) :discard)))
    (take-credits state :corp)
    ; Turn 3 Runner
    (take-credits state :runner)
    ; Turn 4 Corp
    (is (= 4 (:agenda-point (get-corp)))) ; PS2 should get scored
    (is (= (:zone (refresh publics2) :scored)))
    (is (= 12 (:credit (get-corp))) "twice Adonis money and 2xmoney turn, no third Adonis")
    )))