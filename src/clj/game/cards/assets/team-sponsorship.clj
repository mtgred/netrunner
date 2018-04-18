(in-ns 'game.core)

(def card-definitions-assets-team-sponsorship
  {"Team Sponsorship"
   {:events {:agenda-scored {:label "Install a card from Archives or HQ"
                             :prompt "Select a card from Archives or HQ to install"
                             :show-discard true
                             :interactive (req true)
                             :delayed-completion true
                             :choices {:req #(and (not (is-type? % "Operation"))
                                                  (= (:side %) "Corp")
                                                  (#{[:hand] [:discard]} (:zone %)))}
                             :msg (msg (corp-install-msg target))
                             :effect (effect (corp-install eid target nil {:no-install-cost true}))}}}})
