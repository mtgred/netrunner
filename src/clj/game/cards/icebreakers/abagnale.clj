(in-ns 'game.cards.icebreakers)

(def card-definition-abagnale
  {"Abagnale"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 2 2)
                                 {:label "Bypass Code Gate being encountered"
                                  :req (req (has-subtype? current-ice "Code Gate"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :effect (effect (trash card {:cause :ability-cost}))}]})})
