(in-ns 'game.core)

(def card-definitions-events-credit-kiting
  {"Credit Kiting"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :prompt "Select a card to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program")
                             (is-type? % "Resource"))
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -8])
                    (runner-install target)
                    (tag-runner 1))}})
