(in-ns 'game.cards.assets)

(def card-definition-technoco
  {"TechnoCo"
   (letfn [(is-techno-target [card]
             (or (is-type? card "Program")
                 (is-type? card "Hardware")
                 (and (is-type? card "Resource") (has-subtype? card "Virtual"))))]
     {:events {:pre-install {:req (req (and (is-techno-target target)
                                            (not (second targets)))) ; not facedown
                             :effect (effect (install-cost-bonus [:credit 1]))}
               :runner-install {:req (req (and (is-techno-target target)
                                               (not (second targets)))) ; not facedown
                                :msg "gain 1 [Credits]"
                                :effect (req (gain-credits state :corp 1))}}})})
