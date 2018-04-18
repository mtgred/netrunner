(in-ns 'game.core)

(def card-definitions-events-planned-assault
  {"Planned Assault"
   {:msg (msg "play " (:title target))
    :choices (req (cancellable (filter #(and (has-subtype? % "Run")
                                             (<= (:cost %) (:credit runner))) (:deck runner)) :sorted))
    :prompt "Choose a Run event" :effect (effect (trigger-event :searched-stack nil)
                                                 (shuffle! :deck)
                                                 (play-instant target {:no-additional-cost true}))}})
