(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-technoco
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
                                :effect (req (gain state :corp :credit 1))}}})})