(in-ns 'game.cards.icebreakers)

(def card-definition-mass-driver
  {"Mass-Driver"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "Prevention of subroutine resolution on next ICE is manual"
                     :abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 1 1)]})})
