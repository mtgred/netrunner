(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-gabriel-santiago-consummate-professional
  {"Gabriel Santiago: Consummate Professional"
   {:events {:successful-run {:silent (req true)
                              :req (req (and (= target :hq)
                                             (first-successful-run-on-server? state :hq)))
                              :msg "gain 2 [Credits]"
                              :effect (effect (gain :credit 2)) }}}})
