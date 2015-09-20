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
      (is (= 3 (:counter (core/get-card state kati))) "Store 3cr on Kati")
      (card-ability state :runner (core/get-card state kati) 0)
      (is (= 2 (:click (get-runner))) "Second use of Kati should not be allowed")
      (is (= 3 (:counter (core/get-card state kati))) "Second use of Kati should not be allowed")
      (take-credits state :runner 2)
      (is (= 5 (:credit (get-runner))) "Pass turn, take 2cr")
      (take-credits state :corp)
      (card-ability state :runner (core/get-card state kati) 0)
      (is (= 6 (:counter (core/get-card state kati))) "Store 3cr more on Kati")
      (take-credits state :runner 3)
      (is (= 8 (:credit (get-runner))) "Pass turn, take 3cr")
      (take-credits state :corp)
      (card-ability state :runner (core/get-card state kati) 1)
      (is (= 14 (:credit (get-runner))) "Take 6cr from Kati")
      (is (zero? (:counter (core/get-card state kati))) "No counters left on Kati"))))