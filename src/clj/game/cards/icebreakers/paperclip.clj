(in-ns 'game.cards.icebreakers)

(def card-definition-paperclip
  {"Paperclip"
   (conspiracy "Paperclip" "Barrier"
               [{:label (str "X [Credits]: +X strength, break X subroutines")
                 :choices {:number (req (:credit runner))
                           :default (req (if (:current-strength current-ice)
                                           (max (- (:current-strength current-ice)
                                                   (:current-strength card))
                                                1)
                                           1))}
                 :prompt "How many credits?"
                 :effect (effect (lose-credits target)
                                 (pump card target))
                 :msg (msg "spend " target " [Credits], increase strength by " target ", and break "
                           (quantify target "Barrier subroutine"))}])})
