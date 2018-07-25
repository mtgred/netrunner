(in-ns 'game.cards.agendas)

(def card-definition-broad-daylight
  {"Broad Daylight"
   (letfn [(add-counters [state side card eid]
             (let [bp (get-in @state [:corp :bad-publicity] 0)]
               (add-counter state :corp card :agenda bp)
               (effect-completed state side eid)))]
     {:prompt "Take 1 bad publicity?"
      :choices ["Yes" "No"]
      :async true
      :effect (req (if (= target "Yes")
                     (wait-for (gain-bad-publicity state :corp 1)
                               (system-msg state :corp "used Broad Daylight to take 1 bad publicity")
                               (add-counters state side card eid))
                     (add-counters state side card eid)))
      :abilities [{:cost [:click 1] :counter-cost [:agenda 1]
                   :async true
                   :label "Do 2 meat damage"
                   :once :per-turn
                   :msg "do 2 meat damage"
                   :effect (effect (damage eid :meat 2 {:card card}))}]})})
