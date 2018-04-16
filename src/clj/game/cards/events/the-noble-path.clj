(in-ns 'game.core)

(declare run-event)

(def card-events-the-noble-path
  {"The Noble Path"
   {:effect (req (doseq [c (:hand runner)]
                   (trash state side c))
                 (register-events state side
                                  {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                (damage-prevent :meat Integer/MAX_VALUE)
                                                                (damage-prevent :brain Integer/MAX_VALUE))}
                                   :run-ends {:effect (effect (unregister-events card))}}
                                  (assoc card :zone '(:discard)))
                 (resolve-ability state side
                   {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :msg (msg "trash their Grip and make a run on " target ", preventing all damage")
                    :effect (req (let [runtgt [(last (server->zone state target))]
                                       ices (get-in @state (concat [:corp :servers] runtgt [:ices]))]
                                   (swap! state assoc :per-run nil
                                                      :run {:server runtgt :position (count ices)
                                                            :access-bonus 0 :run-effect nil})
                                   (gain-run-credits state :runner (:bad-publicity corp))
                                   (swap! state update-in [:runner :register :made-run] #(conj % (first runtgt)))
                                   (trigger-event state :runner :run runtgt)))} card nil))
    :events {:pre-damage nil :run-ends nil}}})