(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-jak-sinclair
  {"Jak Sinclair"
   (let [ability {:label "Make a run (start of turn)"
                  :prompt "Choose a server to run with Jak Sinclair"
                  :once :per-turn
                  :req (req (:runner-phase-12 @state))
                  :choices (req runnable-servers)
                  :msg (msg "make a run on " target " during which no programs can be used")
                  :makes-run true
                  :effect (effect (run target))}]
   {:implementation "Doesn't prevent program use"
    :flags {:runner-phase-12 (req true)}
    :install-cost-bonus (req [:credit (* -1 (:link runner))])
    :events {:runner-turn-begins
              {:optional {:req (req (not (get-in @state [:per-turn (:cid card)])))
                          :prompt "Use Jak Sinclair to make a run?"
                          :yes-ability ability}}}
    :abilities [ability]})})