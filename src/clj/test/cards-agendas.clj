(in-ns 'test.core)

(deftest ancestral-imager
  "Ancestral Imager - damage on jack out"
  (do-game
    (new-game (default-corp [(qty "Ancestral Imager" 3)]) (default-runner))
    (play-from-hand state :corp "Ancestral Imager" "New remote")
    (let [ai (get-in @state [:corp :servers :remote1 :content 0])]
      (core/advance state :corp {:card (refresh ai)})
      (core/advance state :corp {:card (refresh ai)})
      (take-credits state :corp)
      (take-credits state :runner)
      (core/advance state :corp {:card (refresh ai)})
      (is (= 3 (get-in (refresh ai) [:advance-counter])))
      (core/score state :corp {:card (refresh ai)})
      (take-credits state :corp)
      (is (= 3 (count(get-in @state [:runner :hand]))))
      (core/click-run state :runner {:server :hq})
      (core/jack-out state :runner nil)
      (is (= 2 (count(get-in @state [:runner :hand]))))
      )))

(deftest fetal-ai-damage
  "Fetal AI - damage on access"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
    (play-from-hand state :corp "Fetal AI" "New remote")
    (take-credits state :corp 2)
    (core/click-run state :runner {:server :remote1})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Yes")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
    (is (= 3 (:credit (get-runner))) "Runner paid 2cr to steal Fetal AI")
    (is (= 1 (count (:scored (get-runner)))) "Runner stole Fetal AI")))

(deftest fetal-ai-cant-afford
  "Fetal AI - can't afford to steal"
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
    (play-from-hand state :corp "Fetal AI" "New remote")
    (take-credits state :corp 2)
    (core/lose state :runner :credit 5)
    (core/click-run state :runner {:server :remote1})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (prompt-choice :runner "Yes")
    (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
    (is (= 0 (count (:scored (get-runner)))) "Runner could not steal Fetal AI")))