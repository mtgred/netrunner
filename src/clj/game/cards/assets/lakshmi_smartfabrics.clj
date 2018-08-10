(in-ns 'game.cards.assets)

(def card-definition-lakshmi-smartfabrics
  {"Lakshmi Smartfabrics"
   {:events {:rez {:effect (effect (add-counter card :power 1))}}
    :abilities [{:req (req (seq (filter #(and (is-type? % "Agenda")
                                              (>= (get-counters card :power)
                                                  (:agendapoints %)))
                                        (:hand corp))))
                 :label "X power counters: Reveal an agenda worth X points from HQ"
                 :effect (req (let [c (get-counters card :power)]
                                (resolve-ability
                                  state side
                                  {:prompt "Select an agenda in HQ to reveal"
                                   :choices {:req #(and (is-type? % "Agenda")
                                                        (>= c (:agendapoints %)))}
                                   :msg (msg "reveal " (:title target) " from HQ")
                                   :effect (req (let [title (:title target)
                                                      pts (:agendapoints target)]
                                                  (register-turn-flag! state side
                                                    card :can-steal
                                                    (fn [state side card]
                                                      (if (= (:title card) title)
                                                        ((constantly false)
                                                         (toast state :runner "Cannot steal due to Lakshmi Smartfabrics." "warning"))
                                                        true)))
                                                  (add-counter state side card :power (- pts))))} card nil)))}]}})
