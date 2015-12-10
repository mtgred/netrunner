(in-ns 'test.core)

(deftest cerebral-imaging-max-hand-size
  "Cerebral Imaging - Maximum hand size equal to credits"
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Has 13 credits")
    (is (= 13 (:max-hand-size (get-corp))) "Max hand size is 13")))

(deftest iain-stirling-credits
  "Iain Stirling - Gain 2 credits when behind"
  (do-game
    (new-game
      (default-corp [(qty "Breaking News" 1)])
      (make-deck "Iain Stirling: Retired Spook" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (take-credits state :corp)
      (is (= 1 (:agenda-point (get-corp))))
      (take-credits state :runner 1)
      (is (= 8 (:credit (get-runner))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-trash-cost
  "Industrial Genomics - Increase trash cost"
  (do-game
    (new-game
      (make-deck "Industrial Genomics: Growing Solutions" [(qty "PAD Campaign" 3)
                                                           (qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (core/move state :corp (find-card "PAD Campaign" (:hand (get-corp))) :discard)
    (core/move state :corp (find-card "PAD Campaign" (:hand (get-corp))) :discard)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :discard)
    (let [pad (get-in @state [:corp :servers :remote1 :content 0])]
      (core/rez state :corp pad)
      (take-credits state :corp)
      (core/click-run state :runner {:server "Server 1"})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (is (= 8 (core/trash-cost state :runner (refresh pad)))))))

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

(deftest ken-tenma-run-event-credit
  "Ken 'Express' Tenma - Gain 1 credit when first Run event played"
  (do-game
    (new-game (default-corp) (make-deck "Ken \"Express\" Tenma: Disappeared Clone" [(qty "Account Siphon" 2)]))
    (take-credits state :corp)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit for first Run event")
    (prompt-choice :runner "Run ability")
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 16 (:credit (get-runner))) "No credit gained for second Run event")))

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

(deftest quetzal-ability
  "Quetzal ability- once per turn"
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Quetzal: Free Spirit" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (let [q (get-in @state [:runner :identity])
          qdef (core/card-def (get-in @state [:runner :identity]))]
      (card-ability state :runner q 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg])))
      (core/click-credit state :runner nil)
      (card-ability state :runner (refresh q) 0)
      (is (not (last-log-contains? state (get-in qdef [:abilities 0 :msg]))))
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-credit state :runner nil)
      (card-ability state :runner (refresh q) 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg])))
      )))

(deftest reina-rez-cost-increase
  "Reina Roja - Increase cost of first rezzed ICE"
  (do-game
    (new-game
      (default-corp [(qty "Quandary" 3)])
      (make-deck "Reina Roja: Freedom Fighter" []))
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (core/click-run state :runner {:server "R&D"})
    (let [quan (get-in @state [:corp :servers :rd :ices 0])]
      (core/rez state :corp quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))

(deftest spark-advertisements
  "Spark Agency - Rezzing advertisements"
  (do-game
    (new-game
      (make-deck "Spark Agency: Worldswide Reach" [(qty "Launch Campaign" 2)])
      (default-runner))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [lc1 (get-in @state [:corp :servers :remote1 :content 0])
          lc2 (get-in @state [:corp :servers :remote2 :content 0])]
      (core/rez state :corp lc1)
      (is (= 4 (:credit (get-runner))) "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (take-credits state :corp)
      (core/click-run state :runner {:server "Server 1"})
      (core/rez state :corp lc2)
      (is (= 3 (:credit (get-runner))) "Runner lost 1 credit from rez of advertisement (Runner turn)"))))

(deftest titan-agenda-counter
  "Titan Transnational - Add a counter to a scored agenda"
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)])
      (default-runner))
    (play-from-hand state :corp "Project Atlas" "New remote")
    (let [atl (get-in @state [:corp :servers :remote1 :content 0])]
      (core/gain state :corp :click 1)
      (core/advance state :corp {:card (refresh atl)})
      (core/advance state :corp {:card (refresh atl)})
      (core/advance state :corp {:card (refresh atl)})
      (core/score state :corp {:card (refresh atl)})
      (let [scored (get-in @state [:corp :scored 0])]
        (is (= 1 (:counter scored)) "1 counter added by Titan")))))
