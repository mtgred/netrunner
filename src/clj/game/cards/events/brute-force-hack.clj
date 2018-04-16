(in-ns 'game.core)

(declare run-event)

(def card-events-brute-force-hack
  {"Brute-Force-Hack"
   {:implementation "Runner must calculate the right number of credits including other game effects for the planned target ICE"
    :prompt "How many [Credits]?" :choices :credit
    :effect (effect (system-msg (str "spends " target " [Credit] on Brute-Force-Hack"))
                    (resolve-ability {:choices {:req #(and (ice? %)
                                                           (rezzed? %)
                                                           (<= (:cost %) target))}
                                      :effect (effect (derez target))
                                      :msg (msg "derez " (:title target))} card nil))}})