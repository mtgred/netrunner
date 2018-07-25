(in-ns 'game.cards.agendas)

(def card-definition-next-wave-2
  {"NEXT Wave 2"
   {:not-when-scored true
    :req (req (some #(and (rezzed? %)
                          (ice? %)
                          (has-subtype? % "NEXT"))
                    (all-installed state :corp)))
    :optional {:prompt "Do 1 brain damage with NEXT Wave 2?"
               :yes-ability {:msg "do 1 brain damage"
                             :effect (effect (damage eid :brain 1 {:card card}))}}}})
