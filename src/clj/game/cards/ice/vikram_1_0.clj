(in-ns 'game.cards.ice)

(def card-definition-vikram-1-0
  {"Vikram 1.0"
   {:implementation "Program prevention is not implemented"
    :subroutines [{:msg "prevent the Runner from using programs for the remainder of this run"}
                  (trace-ability 4 (do-brain-damage 1))]
    :runner-abilities [(runner-break [:click 1] 1)]}})
