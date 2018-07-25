(in-ns 'game.cards.ice)

(def card-definition-ravana-1-0
  {"Ravana 1.0"
   {:subroutines [{:label "Resolve a subroutine on another piece of rezzed bioroid ICE"
                   :choices {:req #(and (rezzed? %) (ice? %) (has-subtype? % "Bioroid"))}
                   :msg (msg "resolve a subroutine on " (:title target))}]
    :runner-abilities [(runner-break [:click 1] 1)]}})
