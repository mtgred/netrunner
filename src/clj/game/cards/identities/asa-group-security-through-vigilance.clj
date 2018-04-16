(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-asa-group-security-through-vigilance
  {"Asa Group: Security Through Vigilance"
   {:events {:corp-install
             {:delayed-completion true
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
                                :delayed-completion true
                                :choices {:req #(and (in-hand? %)
                                                     (= (:side %) "Corp")
                                                     (corp-installable-type? %)
                                                     (not (is-type? % "Agenda"))
                                                     (or (is-remote? z)
                                                         (ice? %)))}
                                :effect (effect (corp-install eid target (zone->name z) nil))}
                               card nil)))}}}})