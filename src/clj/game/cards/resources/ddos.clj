(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-ddos
  {"DDoS"
   {:abilities [{:msg "prevent the corp from rezzing the outermost piece of ice during a run on any server this turn"
                 :effect (effect
                           (register-turn-flag!
                             card :can-rez
                             (fn [state side card]
                               (if (and (ice? card)
                                        (= (count (get-in @state (concat [:corp :servers] (:server (:run @state)) [:ices])))
                                           (inc (ice-index state card))))
                                 ((constantly false) (toast state :corp "Cannot rez any outermost ICE due to DDoS." "warning"))
                                 true)))
                           (trash card {:cause :ability-cost}))}]}})