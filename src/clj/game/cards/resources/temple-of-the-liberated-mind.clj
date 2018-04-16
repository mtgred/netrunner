(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-temple-of-the-liberated-mind
  {"Temple of the Liberated Mind"
   {:abilities [{:cost [:click 1]
                 :label "Place 1 power counter"
                 :msg "place 1 power counter on it"
                 :effect (effect (add-counter card :power 1))}
                {:label "Gain [Click]"
                 :counter-cost [:power 1]
                 :req (req (= (:active-player @state) :runner))
                 :msg "gain [Click]" :once :per-turn
                 :effect (effect (gain :click 1))}]}})