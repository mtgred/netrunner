(in-ns 'game.core)

(def card-definitions-icebreakers-demara
  {"Demara"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 2 3)
                                 {:label "Bypass Barrier being encountered"
                                  :req (req (has-subtype? current-ice "Barrier"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :effect (effect (trash card {:cause :ability-cost}))}]})})
