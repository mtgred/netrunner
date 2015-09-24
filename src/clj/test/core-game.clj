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
