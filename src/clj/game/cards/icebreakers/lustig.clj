(in-ns 'game.cards.icebreakers)

(def card-definition-lustig
  {"Lustig"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)
                                 {:label "Bypass Sentry being encountered"
                                  :req (req (has-subtype? current-ice "Sentry"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :effect (effect (trash card {:cause :ability-cost}))}]})})
