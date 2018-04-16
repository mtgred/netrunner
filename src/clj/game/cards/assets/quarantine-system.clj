(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-quarantine-system
  {"Quarantine System"
   (letfn [(rez-ice [cnt] {:prompt "Select an ICE to rez"
                           :delayed-completion true
                           :choices {:req #(and (ice? %) (not (rezzed? %)))}
                           :msg (msg "rez " (:title target))
                           :effect (req (let [agenda (last (:rfg corp))
                                              ap (:agendapoints agenda 0)]
                                          (rez-cost-bonus state side (* ap -2))
                                          (rez state side target {:no-warning true})
                                          (if (< cnt 3) (continue-ability state side (rez-ice (inc cnt)) card nil)
                                                        (effect-completed state side eid))))})]
     {:abilities [{:label "Forfeit agenda to rez up to 3 ICE with a 2 [Credit] discount per agenda point"
                   :req (req (pos? (count (:scored corp))))
                   :cost [:forfeit]
                   :effect (req (continue-ability state side (rez-ice 1) card nil))}]})})
