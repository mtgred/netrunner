(in-ns 'test.core)

(deftest minigame-prevent-netdmg-resourcetrash
  "Mini-game testing prevention of net damage and resource trashing, with hosted Fall Guy"
  (do-game
    (new-game
      (default-corp [(qty "Neural EMP" 1) (qty "Hedge Fund" 3) (qty "SEA Source" 1)])
      (default-runner [(qty "Fall Guy" 1) (qty "Off-Campus Apartment" 1) (qty "Net Shield" 1)
                       (qty "Wireless Net Pavilion" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp 1)
    (is (= 14 (:credit (get-corp))))
    (core/gain state :runner :click 2)
    (core/click-run state :runner {:server :archives})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil) ; enable Corp play of Neural and SEA next turn
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Off-Campus Apartment")
    (play-from-hand state :runner "Wireless Net Pavilion")
    (play-from-hand state :runner "Net Shield")
    (let [apt (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner apt 0)
      (prompt-select :runner (find-card "Fall Guy" (:hand (get-runner))))
      (let [fg (first (:hosted (refresh apt)))]
        (take-credits state :runner)
        (is (= 6 (:credit (get-runner))))
        (play-from-hand state :corp "Neural EMP")
        (let [ns (get-in @state [:runner :rig :program 0])]
          (card-ability state :runner ns 0)
          (is (= 5 (:credit (get-runner))) "Runner paid 1c to survive Neural EMP")
          (prompt-choice :runner "Done")
          (play-from-hand state :corp "SEA Source")
          (prompt-choice :corp 3) ; boost trace to 6
          (prompt-choice :runner 0)
          (is (= 1 (:tag (get-runner))) "Runner took tag from SEA Source")
          (is (= 7 (:credit (get-corp))))
          (core/trash-resource state :corp nil)
          (prompt-select :corp (find-card "Off-Campus Apartment" (:rig (get-runner))))
          (is (= 3 (:credit (get-corp))) "WNP increased cost to trash a resource by 2")
          (card-ability state :runner fg 0) ; Trash Fall Guy to save the Apartment!
          (is (= (:title (get-in @state [:runner :rig :resource 0])) "Off-Campus Apartment") "Apartment still standing")
          (is (= (:title (last (:discard (get-runner)))) "Fall Guy") "Fall Guy trashed"))))))
