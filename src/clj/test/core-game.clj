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
              (default-runner [(qty "Corroder" 1) (qty "Desperado" 1) (qty "Paparazzi" 1)]))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Jackson Howard")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/end-turn state :corp nil)
    (core/start-turn state :runner nil)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Desperado")
    (play-from-hand state :runner "Paparazzi")
    (let [hqiwall0 (get-in @state [:corp :servers :hq :ices 0])
          hqiwall1 (get-in @state [:corp :servers :hq :ices 1])
          rdiwall (get-in @state [:corp :servers :rd :ices 0])
          jh1 (first (get-in @state [:corp :servers :remote0 :content]))
          jh2 (first (get-in @state [:corp :servers :remote1 :content]))
          corr (get-in @state [:runner :rig :program 0])
          desp (get-in @state [:runner :rig :hardware 0])
          pap (get-in @state [:runner :rig :resource 0])]
      (core/rez state :corp hqiwall0)
      (core/rez state :corp jh1)
      (is (= (core/card-str hqiwall0) "Ice Wall protecting HQ at position 0"))
      (is (= (core/card-str hqiwall1) "unrezzed ICE protecting HQ at position 1"))
      (is (= (core/card-str rdiwall) "unrezzed ICE protecting R&D at position 0"))
      (is (= (core/card-str jh1) "Jackson Howard in Server 1"))
      (is (= (core/card-str jh2) "unrezzed card in Server 2"))
      (is (= (core/card-str corr) "Corroder"))
      (is (= (core/card-str desp) "Desperado"))
      (is (= (core/card-str pap) "Paparazzi"))
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