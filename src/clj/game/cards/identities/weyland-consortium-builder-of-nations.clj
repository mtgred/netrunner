(in-ns 'game.core)

(def card-definitions-identities-weyland-consortium-builder-of-nations
  {"Weyland Consortium: Builder of Nations"
   {:implementation "Damage triggered manually"
    :abilities [{:label "Do 1 meat damage"
                 :once :per-turn
                 :prompt "Do a meat damage from identity ability?"
                 :choices (cancellable ["Yes"])
                 :delayed-completion true
                 :effect (req (when (= target "Yes")
                                (damage state side eid :meat 1 {:card card})
                                (system-msg state side "uses Weyland Consortium: Builder of Nations to do 1 meat damage")))}]}})
