(in-ns 'game.core)

(def card-hardware-rubicon-switch
  {"Rubicon Switch"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :delayed-completion true
                 :prompt "How many [Credits]?" :choices :credit
                 :effect (effect (system-msg (str "spends a [Click] and " target " [Credit] on Rubicon Switch"))
                                 (resolve-ability {:choices {:req #(and (ice? %)
                                                                        (= :this-turn (:rezzed %))
                                                                        (<= (:cost %) target))}
                                                   :effect (effect (derez target))
                                                   :msg (msg "derez " (:title target))} card nil))}]}})
