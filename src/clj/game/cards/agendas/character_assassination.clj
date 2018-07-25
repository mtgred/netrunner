(in-ns 'game.cards.agendas)

(def card-definition-character-assassination
  {"Character Assassination"
   {:prompt "Select a resource to trash"
    :choices {:req #(and (installed? %)
                         (is-type? % "Resource"))}
    :msg (msg "trash " (:title target))
    :interactive (req true)
    :async true
    :effect (effect (trash eid target {:unpreventable true}))}})
