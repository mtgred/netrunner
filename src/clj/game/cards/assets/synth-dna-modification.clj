(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-synth-dna-modification
  {"Synth DNA Modification"
   {:implementation "Manual fire once subroutine is broken"
    :abilities [{:msg "do 1 net damage"
                 :label "Do 1 net damage after AP subroutine broken"
                 :once :per-turn
                 :effect (effect (damage eid :net 1 {:card card}))}]}})