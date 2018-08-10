(in-ns 'game.cards.icebreakers)

(def card-definition-dai-v
  {"Dai V"
   (auto-icebreaker ["All"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [{:cost [:credit 2]
                                  :msg "break all ICE subroutines (using stealth [Credits])"}
                                 (strength-pump 1 1)]})})
