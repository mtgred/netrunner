(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-sensor-net-activation
  {"Sensor Net Activation"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1]
                 :req (req (some #(and (has-subtype? % "Bioroid") (not (rezzed? %))) (all-installed state :corp)))
                 :prompt "Choose a bioroid to rez, ignoring all costs"
                 :choices {:req #(and (has-subtype? % "Bioroid") (not (rezzed? %)))}
                 :msg (msg "rez " (card-str state target) ", ignoring all costs")
                 :effect (req (let [c target]
                                (rez state side c {:ignore-cost :all-costs})
                                (register-events state side
                                  {:corp-turn-ends {:effect (effect (derez c)
                                                                    (unregister-events card))}
                                   :runner-turn-ends {:effect (effect (derez c)
                                                                      (unregister-events card))}} card)))}]
      :events {:corp-turn-ends nil :runner-turn-ends nil}}})