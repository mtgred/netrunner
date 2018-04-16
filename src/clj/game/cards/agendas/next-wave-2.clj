(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-next-wave-2
  {"NEXT Wave 2"
   {:delayed-completion true
    :not-when-scored true
    :effect (req (if (some #(and (rezzed? %)
                                 (ice? %)
                                 (has-subtype? % "NEXT"))
                           (all-installed state :corp))
                   (continue-ability state side
                     {:optional
                      {:prompt "Do 1 brain damage with NEXT Wave 2?"
                       :yes-ability {:msg "do 1 brain damage"
                                     :effect (effect (damage eid :brain 1 {:card card}))}
                       :no-ability {:effect (req (effect-completed state side eid))}}}
                    card nil)
                   (effect-completed state side eid)))}})