(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-ice-analyzer
  {"Ice Analyzer"
   {:implementation "Credit use restriction is not enforced"
    :events {:rez {:req (req (ice? target))
                   :msg "place 1 [Credits] on Ice Analyzer"
                   :effect (effect (add-counter :runner card :credit 1))}}
    :abilities [{:counter-cost [:credit 1]
                 :effect (effect (gain :credit 1))
                 :msg "take 1 [Credits] to install programs"}]}})