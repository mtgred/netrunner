(in-ns 'game.core)

(def card-operations-power-grid-overload
  {"Power Grid Overload"
   {:req (req (last-turn? state :runner :made-run))
    :trace {:base 2
            :msg "trash 1 piece of hardware"
            :delayed-completion true
            :effect (req (let [max-cost (- target (second targets))]
                           (continue-ability state side
                                             {:choices {:req #(and (is-type? % "Hardware")
                                                                   (<= (:cost %) max-cost))}
                                              :msg (msg "trash " (:title target))
                                              :effect (effect (trash target))}
                                             card nil))
                         (system-msg state :corp (str "trashes 1 piece of hardware with install cost less than or equal to " (- target (second targets)))))}}})