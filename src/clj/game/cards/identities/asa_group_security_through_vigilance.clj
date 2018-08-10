(in-ns 'game.cards.identities)

(def card-definition-asa-group-security-through-vigilance
  {"Asa Group: Security Through Vigilance"
   {:events {:corp-install
             {:async true
              :req (req (first-event? state :corp :corp-install))
              :effect (req (let [installed-card target
                                 z (butlast (:zone installed-card))]
                             (continue-ability
                               state side
                               {:prompt (str "Select a "
                                             (if (is-remote? z)
                                               "non-agenda"
                                               "piece of ice")
                                             " in HQ to install with Asa Group: Security Through Vigilance (optional)")
                                :choices {:req #(and (in-hand? %)
                                                     (= (:side %) "Corp")
                                                     (corp-installable-type? %)
                                                     (not (is-type? % "Agenda"))
                                                     (or (is-remote? z)
                                                         (ice? %)))}
                                :effect (effect (corp-install eid target (zone->name z) nil))}
                               card nil)))}}}})
