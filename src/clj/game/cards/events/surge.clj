(in-ns 'game.core)

(def card-definitions-events-surge
  {"Surge"
   {:msg (msg "place 2 virus tokens on " (:title target))
    :choices {:req #(and (has-subtype? % "Virus") (:added-virus-counter %))}
    :effect (req (add-counter state :runner target :virus 2))}})
