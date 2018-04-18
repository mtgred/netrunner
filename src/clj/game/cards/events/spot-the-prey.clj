(in-ns 'game.core)

(def card-definitions-events-spot-the-prey
  {"Spot the Prey"
   {:prompt "Select 1 non-ICE card to expose"
    :msg "expose 1 card and make a run"
    :choices {:req #(and (installed? %) (not (ice? %)) (= (:side %) "Corp"))}
    :delayed-completion true
    :effect (req (when-completed (expose state side target)
                                 (continue-ability
                                   state side
                                   {:prompt "Choose a server"
                                    :choices (req runnable-servers)
                                    :delayed-completion true
                                    :effect (effect (game.core/run eid target))}
                                   card nil)))}})
