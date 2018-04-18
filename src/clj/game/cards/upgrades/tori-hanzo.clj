(in-ns 'game.core)

(def card-definitions-upgrades-tori-hanzo
  {"Tori Hanzō"
   {:events
    {:pre-resolve-damage
     {:once :per-run
      :delayed-completion true
      :req (req (and this-server (= target :net) (> (last targets) 0) (can-pay? state :corp nil [:credit 2])))
      :effect (req (swap! state assoc-in [:damage :damage-replace] true)
                   (damage-defer state side :net (last targets))
                   (show-wait-prompt state :runner "Corp to use Tori Hanzō")
                   (continue-ability state side
                     {:optional {:prompt (str "Pay 2 [Credits] to do 1 brain damage with Tori Hanzō?") :player :corp
                                 :yes-ability {:delayed-completion true
                                               :msg "do 1 brain damage instead of net damage"
                                               :effect (req (swap! state update-in [:damage] dissoc :damage-replace :defer-damage)
                                                            (clear-wait-prompt state :runner)
                                                            (pay state :corp card :credit 2)
                                                            (when-completed (damage state side :brain 1 {:card card})
                                                                            (do (swap! state assoc-in [:damage :damage-replace] true)
                                                                                (effect-completed state side eid))))}
                                 :no-ability {:delayed-completion true
                                              :effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                                           (clear-wait-prompt state :runner)
                                                           (effect-completed state side eid))}}} card nil))}
     :prevented-damage {:req (req (and this-server (= target :net) (> (last targets) 0)))
                        :effect (req (swap! state assoc-in [:per-run (:cid card)] true))}}}})
