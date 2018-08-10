(in-ns 'game.cards.icebreakers)

(def card-definition-sage
  {"Sage"
   (ancient-greek-breaker "sage" [{:cost [:credit 2] :req (req (or (has-subtype? current-ice "Barrier")
                                                                   (has-subtype? current-ice "Code Gate")))
                                   :msg "break 1 Code Gate or Barrier subroutine"}])})
