(in-ns 'game.cards.programs)

(def card-definition-consume
  {"Consume"
   {:events {:runner-trash {:async true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :effect (req (let [amt-trashed (count (filter #(card-is? % :side :corp) targets))
                                               auto-ab {:effect (effect (add-counter :runner card :virus amt-trashed))
                                                        :msg (str "place " (quantify amt-trashed "virus counter") " on Consume")}
                                               sing-ab {:optional {:prompt "Place a virus counter on Consume?"
                                                                   :yes-ability {:effect (effect (add-counter :runner card :virus 1))
                                                                                 :msg "place 1 virus counter on Consume"}}}
                                               mult-ab {:prompt "Place virus counters on Consume?"
                                                        :choices {:number (req amt-trashed)
                                                                  :default (req amt-trashed)}
                                                        :msg (msg "place " (quantify target "virus counter") " on Consume")
                                                        :effect (effect (add-counter :runner card :virus target))}
                                               ab (if (> amt-trashed 1) mult-ab sing-ab)
                                               ab (if (get-in card [:special :auto-accept]) auto-ab ab)]
                                           (continue-ability state side ab card targets)))}}
    :effect (effect (toast "Tip: You can toggle automatically adding virus counters by clicking Consume."))
    :abilities [{:req (req (pos? (get-virus-counters state side card)))
                 :cost [:click 1]
                 :label "Gain 2 [Credits] for each hosted virus counter, then remove all virus counters."
                 :effect (req (gain-credits state side (* 2 (get-virus-counters state side card)))
                              (update! state side (assoc-in card [:counter :virus] 0))
                              (when-let [hiveminds (filter #(= "Hivemind" (:title %)) (all-active-installed state :runner))]
                                        (doseq [h hiveminds]
                                               (update! state side (assoc-in h [:counter :virus] 0)))))
                 :msg (msg (let [local-virus (get-counters card :virus)
                                 global-virus (get-virus-counters state side card)
                                 hivemind-virus (- global-virus local-virus)]
                             (str "gain " (* 2 global-virus) " [Credits], removing " (quantify local-virus "virus counter") " from Consume"
                             (when (pos? hivemind-virus)
                                   (str " (and " hivemind-virus " from Hivemind)")))))}
                {:effect (effect (update! (update-in card [:special :auto-accept] #(not %)))
                                 (toast (str "Consume will now "
                                             (if (get-in card [:special :auto-accept]) "no longer " "")
                                             "automatically add counters.") "info"))
                 :label "Toggle automatically adding virus counters"}]}})
