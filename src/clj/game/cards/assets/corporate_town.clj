(in-ns 'game.cards.assets)

(def card-definition-corporate-town
  {"Corporate Town"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :additional-cost [:forfeit]
    :flags {:corp-phase-12 (req (and (rezzed? card)
                                     (->> (all-active-installed state :runner)
                                          (filter resource?)
                                          count
                                          pos?)))}
    :abilities [{:label "Trash a resource"
                 :once :per-turn
                 :async true
                 :prompt "Select a resource to trash with Corporate Town"
                 :choices {:req resource?}
                 :msg (msg "trash " (:title target))
                 :effect (effect (trash eid target {:unpreventable true}))}]}})
