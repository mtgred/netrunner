(in-ns 'test.core)

(deftest kate-mac-mccaffrey-discount
  "Kate 'Mac' McCaffrey - Install discount"
  (do-game
    (new-game (default-corp) (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 1 (:credit (get-runner))))))

(deftest kate-mac-mccaffrey-no-discount
  "Kate 'Mac' McCaffrey - No discount for 0 cost"
  (do-game
    (new-game (default-corp) (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)
                                                                                  (qty "Self-modifying Code" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Magnum Opus")
    (is (= 0 (:credit (get-runner))))))

(deftest kate-mac-mccaffrey-discount-cant-afford
  "Kate 'Mac' McCaffrey - Can Only Afford With the Discount"
  (do-game
    (new-game (default-corp) (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :corp)
    (core/lose state :runner :credit 1)
    (is (= 4 (:credit (get-runner))))
    (play-from-hand state :runner "Magnum Opus")
    (is (= 1 (count (get-in @state [:runner :rig :program]))))
    (is (= 0 (:credit (get-runner))))))

(deftest nasir-ability-basic
  "Nasir Ability - Basic"
  (do-game
    (new-game 
      (default-corp [(qty "Ice Wall" 3)]) 
      (make-deck "Nasir Meidan: Cyber Explorer" []))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)

    (core/click-run state :runner {:server "HQ"})
    (let [iwall (get-in @state [:corp :servers :hq :ices 0])
          nasir (get-in @state [:runner :identity])]
      (core/rez state :corp iwall)
      (is (= 5 (:credit (get-runner))))
      (card-ability state :runner nasir 0)
      (is (= 1 (:credit (get-runner)))))))

(deftest nasir-ability-xanadu
  "Nasir Ability - Xanadu"
  (do-game
    (new-game 
      (default-corp [(qty "Ice Wall" 1)]) 
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Xanadu" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)

    (swap! state assoc-in [:runner :credit] 6)
    (play-from-hand state :runner "Xanadu")
    (core/click-run state :runner {:server "HQ"})
    (let [iwall (get-in @state [:corp :servers :hq :ices 0])
          nasir (get-in @state [:runner :identity])]
      (core/rez state :corp iwall)
      (is (= 3 (:credit (get-runner))))
      (card-ability state :runner nasir 0)
      (is (= 2 (:credit (get-runner)))))))