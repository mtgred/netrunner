(in-ns 'game.cards.icebreakers)

(def card-definition-savant
  {"Savant"
   (ancient-greek-breaker "savant" [{:cost [:credit 2] :req (req (has-subtype? current-ice "Sentry"))
                                     :msg "break 1 Sentry subroutine"}
                                    {:cost [:credit 2] :req (req (has-subtype? current-ice "Code Gate"))
                                     :msg "break 2 Code Gate subroutines"}])})
