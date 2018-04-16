(in-ns 'game.core)

(def card-operations-restoring-face
  {"Restoring Face"
   {:prompt "Select a Sysop, Executive or Clone to trash"
    :msg (msg "trash " (card-str state target) " to remove 2 bad publicity")
    :choices {:req #(and (rezzed? %)
                         (or (has-subtype? % "Clone")
                             (has-subtype? % "Executive")
                             (has-subtype? % "Sysop")))}
    :effect (final-effect (lose :bad-publicity 2) (trash target))}})