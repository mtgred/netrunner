(in-ns 'game.core)

(def card-hardware-ramujan-reliant-550-bmi
  {"Ramujan-reliant 550 BMI"
   {:prevent {:damage [:net :brain]}
    :abilities [{:req (req (not-empty (:deck runner)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                                (resolve-ability state side
                                  {:prompt "Choose how much damage to prevent"
                                   :priority 50
                                   :choices {:number (req (min n (count (:deck runner))))}
                                   :msg (msg "trash " (join ", " (map :title (take target (:deck runner))))
                                             " from their Stack and prevent " target " damage")
                                   :effect (effect (damage-prevent :net target)
                                                   (damage-prevent :brain target)
                                                   (mill :runner target)
                                                   (trash card {:cause :ability-cost}))} card nil)))}]}})