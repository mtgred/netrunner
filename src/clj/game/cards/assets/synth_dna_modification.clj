(in-ns 'game.cards.assets)

(def card-definition-synth-dna-modification
  {"Synth DNA Modification"
   {:implementation "Manual fire once subroutine is broken"
    :abilities [{:msg "do 1 net damage"
                 :label "Do 1 net damage after AP subroutine broken"
                 :once :per-turn
                 :effect (effect (damage eid :net 1 {:card card}))}]}})
