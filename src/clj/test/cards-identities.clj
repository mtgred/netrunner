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