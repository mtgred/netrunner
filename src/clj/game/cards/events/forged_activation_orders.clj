(in-ns 'game.cards.events)

(def card-definition-forged-activation-orders
  {"Forged Activation Orders"
   {:choices {:req #(and (ice? %)
                         (not (rezzed? %)))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       icepos (ice-index state ice)]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Rez " (:title ice) " at position " icepos
                                   " of " serv " or trash it?") :choices ["Rez" "Trash"]
                      :effect (effect (resolve-ability
                                        (if (and (= target "Rez") (<= (rez-cost state :corp ice) (:credit corp)))
                                          {:msg (msg "force the rez of " (:title ice))
                                           :effect (effect (rez :corp ice))}
                                          {:msg (msg "trash the ICE at position " icepos " of " serv)
                                           :effect (effect (trash :corp ice))})
                                        card nil))}
                     card nil)))}})
