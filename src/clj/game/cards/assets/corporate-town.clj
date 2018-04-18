(in-ns 'game.core)

(def card-definitions-assets-corporate-town
  {"Corporate Town"
   {:additional-cost [:forfeit]
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    ; not-empty doesn't work for the next line, because it does not return literal true; it returns the collection.
    ; flags need exact equality of value to work.
    :flags {:corp-phase-12 (req (and (pos? (count (filter #(card-is? % :type "Resource") (all-active-installed state :runner))))
                                     (:rezzed card)))}
    :abilities [{:label "Trash a resource"
                 :prompt "Select a resource to trash with Corporate Town"
                 :once :per-turn
                 :choices {:req #(is-type? % "Resource")}
                 :msg (msg "trash " (:title target))
                 :effect (effect (trash target {:unpreventable true}))}]}})
