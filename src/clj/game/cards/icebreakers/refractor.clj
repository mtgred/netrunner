(in-ns 'game.core)

(def card-definitions-icebreakers-refractor
  {"Refractor"
   (auto-icebreaker ["Code Gate"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 3)]})
   "Sadyojata"
   (deva "Sadyojata")})
