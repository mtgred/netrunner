(in-ns 'game.cards.events)

(def card-definition-credit-kiting
  {"Credit Kiting"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :prompt "Select a card to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program")
                             (is-type? % "Resource"))
                         (in-hand? %))}
    :async true
    :effect (req (install-cost-bonus state :runner [:credit -8])
                 (wait-for (runner-install state :runner target nil)
                           (gain-tags state eid :runner 1)))}})
