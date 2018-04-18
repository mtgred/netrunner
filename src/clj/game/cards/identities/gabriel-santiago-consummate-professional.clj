(in-ns 'game.core)

(def card-definitions-identities-gabriel-santiago-consummate-professional
  {"Gabriel Santiago: Consummate Professional"
   {:events {:successful-run {:silent (req true)
                              :req (req (and (= target :hq)
                                             (first-successful-run-on-server? state :hq)))
                              :msg "gain 2 [Credits]"
                              :effect (effect (gain :credit 2)) }}}})
