(in-ns 'game.core)

(def card-definitions-icebreakers-overmind
  {"Overmind"
   (auto-icebreaker ["All"]
                    {:effect (effect (add-counter card :power (:memory runner)))
                     :abilities [{:counter-cost [:power 1]
                                  :msg "break 1 subroutine"}
                                 (strength-pump 1 1)]})
   "Paperclip"
   (conspiracy "Paperclip" "Barrier"
               [{:label (str "X [Credits]: +X strength, break X subroutines")
                 :choices {:number (req (:credit runner))
                           :default (req (if current-ice
                                           (max (- (:current-strength current-ice)
                                                   (:current-strength card))
                                                1)
                                           1))}
                 :prompt "How many credits?"
                 :effect (effect (lose :credit target)
                                 (pump card target))
                 :msg (msg "spend " target " [Credits], increase strength by " target ", and break "
                           (quantify target "Barrier subroutine"))}])})
