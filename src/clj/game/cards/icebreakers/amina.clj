(in-ns 'game.cards.icebreakers)

(def card-definition-amina
  {"Amina"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 3 "Code Gate")
                                 (strength-pump 2 3)
                                 {:label "Corp loses 1 [Credits]"
                                  :req (req (and (has-subtype? current-ice "Code Gate")
                                                 (rezzed? current-ice)))
                                  :msg (msg "make the Corp lose 1 [Credits]")
                                  :effect (effect (lose-credits :corp 1))}]})})
