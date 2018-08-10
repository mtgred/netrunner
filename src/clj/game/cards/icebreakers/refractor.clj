(in-ns 'game.cards.icebreakers)

(def card-definition-refractor
  {"Refractor"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 3)]})})
