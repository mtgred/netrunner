(in-ns 'game.cards.events)

(def card-definition-spot-the-prey
  {"Spot the Prey"
   {:prompt "Select 1 non-ICE card to expose"
    :msg "expose 1 card and make a run"
    :choices {:req #(and (installed? %) (not (ice? %)) (= (:side %) "Corp"))}
    :async true
    :effect (req (wait-for (expose state side target)
                           (continue-ability
                             state side
                             {:prompt "Choose a server"
                              :choices (req runnable-servers)
                              :async true
                              :effect (effect (game.core/run eid target))}
                             card nil)))}})
