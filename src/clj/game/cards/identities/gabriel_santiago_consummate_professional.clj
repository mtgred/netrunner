(in-ns 'game.cards.identities)

(def card-definition-gabriel-santiago-consummate-professional
  {"Gabriel Santiago: Consummate Professional"
   {:events {:successful-run {:silent (req true)
                              :req (req (and (= target :hq)
                                             (first-successful-run-on-server? state :hq)))
                              :msg "gain 2 [Credits]"
                              :effect (effect (gain-credits 2))}}}})
