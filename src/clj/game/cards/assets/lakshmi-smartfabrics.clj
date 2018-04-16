(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-lakshmi-smartfabrics
  {"Lakshmi Smartfabrics"
   {:events {:rez {:effect (effect (add-counter card :power 1))}}
    :abilities [{:req (req (seq (filter #(and (is-type? % "Agenda")
                                              (>= (get-in card [:counter :power] 0)
                                                  (:agendapoints %)))
                                        (:hand corp))))
                 :label "X power counters: Reveal an agenda worth X points from HQ"
                 :effect (req (let [c (get-in card [:counter :power])]
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
