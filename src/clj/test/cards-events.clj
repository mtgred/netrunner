(in-ns 'test.core)

(deftest account-siphon-ability
  "Account Siphon - Use ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :corp) ; pass to runner's turn by taking credits
    (is (= 8 (:credit (get-corp))))

    ; play Account Siphon, use ability
    (play-run-event state (first (:hand (get-runner))) :hq)
    (prompt-choice :runner "Run ability")
    (is (= 2 (:tag (get-runner)))) ; gained 2 tags
    (is (= 15 (:credit (get-runner)))) ; gained 10 credits
    (is (= 3 (:credit (get-corp)))))) ; corp lost 5 credits

(deftest account-siphon-access
  "Account Siphon - Access"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :corp) ; pass to runner's turn by taking credits
    (is (= 8 (:credit (get-corp))))
    ; play another Siphon, do not use ability
    (play-run-event state (first (get-in @state [:runner :hand])) :hq)
    (prompt-choice :runner "Access")
    (is (= 0 (:tag (get-runner)))) ; no new tags
    (is (= 5 (:credit (get-runner)))) ; no change in credits
    (is (= 8 (:credit (get-corp))))))

(deftest sure-gamble
  "Sure Gamble"
  (do-game
    (new-game (default-corp) (default-runner))
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (core/play state :runner {:card (first (:hand (get-runner)))})
    (is (= 9 (:credit (get-runner))))))
