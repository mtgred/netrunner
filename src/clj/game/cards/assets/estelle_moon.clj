(in-ns 'game.cards.assets)

(def card-definition-estelle-moon
  {"Estelle Moon"
   {:events {:corp-install {:req (req (and (#{"Asset" "Agenda" "Upgrade"} (:type target))
                                           (is-remote? (second (:zone target)))))
                            :effect (effect (add-counter card :power 1)
                                            (system-msg (str "places 1 power counter on Estelle Moon")))}}
    :abilities [{:label "Draw 1 card and gain 2 [Credits] for each power counter"
                 :effect (req (let [counters (get-counters card :power)
                                    credits (* 2 counters)]
                                (trash state side card {:cause :ability-cost})
                                (draw state side counters)
                                (gain-credits state side credits)
                                (system-msg state side (str "uses Estelle Moon to draw " counters
                                                            " cards and gain " credits " [Credits]"))))}]}})
