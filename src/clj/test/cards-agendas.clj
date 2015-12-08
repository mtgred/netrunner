(in-ns 'test.core)

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