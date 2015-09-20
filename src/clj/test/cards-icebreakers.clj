(in-ns 'test.core)

(deftest atman-install-2
  "Atman - Installing with 2 power counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (core/resolve-prompt state :runner {:choice 2}) ; pay 2 credits
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 2 (:counter atman)) "2 power counters")
      (is (= 2 (:current-strength atman)) "2 current strength"))))

(deftest atman-install-0
  "Atman - Installing with 0 power counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (core/resolve-prompt state :runner {:choice 0}) ; pay 2 credits
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 0 (:counter atman)) "0 power counters")
      (is (= 0 (:current-strength atman)) "0 current strength"))))