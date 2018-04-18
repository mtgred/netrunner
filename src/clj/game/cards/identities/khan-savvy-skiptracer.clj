(in-ns 'game.core)

(def card-definitions-identities-khan-savvy-skiptracer
  {"Khan: Savvy Skiptracer"
   {:events {:pass-ice
             {:req (req (first-event? state :corp :pass-ice))
              :delayed-completion true
              :effect (req (if (some #(has-subtype? % "Icebreaker") (:hand runner))
                             (continue-ability state side
                                               {:prompt "Select an icebreaker to install from your Grip"
                                                :choices {:req #(and (in-hand? %) (has-subtype? % "Icebreaker"))}
                                                :delayed-completion true
                                                :msg (msg "install " (:title target))
                                                :effect (effect (install-cost-bonus [:credit -1])
                                                                (runner-install eid target nil))}
                                               card nil)
                             (effect-completed state side eid)))}}}})
