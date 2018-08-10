(in-ns 'game.cards.icebreakers)

(def card-definition-adept
  {"Adept"
   (ancient-greek-breaker "adept" [{:cost [:credit 2]
                                    :req (req (or (has-subtype? current-ice "Barrier")
                                                  (has-subtype? current-ice "Sentry")))
                                    :msg "break 1 Sentry or Barrier subroutine"}])})
