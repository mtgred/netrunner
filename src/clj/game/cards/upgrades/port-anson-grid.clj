(in-ns 'game.core)

(def card-definitions-upgrades-port-anson-grid
  {"Port Anson Grid"
   {:msg "prevent the Runner from jacking out unless they trash an installed program"
    :effect (req (when this-server
                   (prevent-jack-out state side)))
    :events {:run {:req (req this-server)
                   :msg "prevent the Runner from jacking out unless they trash an installed program"
                   :effect (effect (prevent-jack-out))}
             :runner-trash {:req (req (and this-server (is-type? target "Program")))
                            :effect (req (swap! state update-in [:run] dissoc :cannot-jack-out))}}}})
