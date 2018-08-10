(in-ns 'game.cards.identities)

(def card-definition-weyland-consortium-builder-of-nations
  {"Weyland Consortium: Builder of Nations"
   {:implementation "Damage triggered manually"
    :abilities [{:label "Do 1 meat damage"
                 :once :per-turn
                 :prompt "Do a meat damage from identity ability?"
                 :choices (cancellable ["Yes"])
                 :async true
                 :effect (req (when (= target "Yes")
                                (damage state side eid :meat 1 {:card card})
                                (system-msg state side "uses Weyland Consortium: Builder of Nations to do 1 meat damage")))}]}})
