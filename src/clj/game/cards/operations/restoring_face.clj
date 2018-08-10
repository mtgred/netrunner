(in-ns 'game.cards.operations)

(def card-definition-restoring-face
  {"Restoring Face"
   {:async true
    :prompt "Select a Sysop, Executive or Clone to trash"
    :msg (msg "trash " (card-str state target) " to remove 2 bad publicity")
    :choices {:req #(and (rezzed? %)
                         (or (has-subtype? % "Clone")
                             (has-subtype? % "Executive")
                             (has-subtype? % "Sysop")))}
    :effect (effect (lose :bad-publicity 2)
                    (trash eid target nil))}})
