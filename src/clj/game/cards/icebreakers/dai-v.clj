(in-ns 'game.core)

(def card-definitions-icebreakers-dai-v
  {"Dai V"
   (auto-icebreaker ["All"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [{:cost [:credit 2]
                                  :msg "break all ICE subroutines (using stealth [Credits])"}
                                 (strength-pump 1 1)]})})
