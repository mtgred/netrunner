(in-ns 'game.core)

(def card-definitions-icebreakers-mass-driver
  {"Mass-Driver"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "Prevention of subroutine resolution on next ICE is manual"
                     :abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 1 1)]})})
