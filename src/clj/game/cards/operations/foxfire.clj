(in-ns 'game.core)

(def card-operations-foxfire
  {"Foxfire"
   {:trace {:base 7
            :prompt "Select 1 card to trash"
            :not-distinct true
            :choices {:req #(and (installed? %)
                                 (or (has-subtype? % "Virtual")
                                     (has-subtype? % "Link")))}
            :msg "trash 1 virtual resource or link"
            :effect (effect (trash target) (system-msg (str "trashes " (:title target))))}}})