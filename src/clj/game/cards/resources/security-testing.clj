(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-security-testing
  {"Security Testing"
   (let [ability {:prompt "Choose a server for Security Testing" :choices (req (conj servers "No server"))
                  :msg (msg "target " target)
                  :req (req (and (not (click-spent? :runner state)) (not (used-this-turn? (:cid card) state))))
                  :effect (req (when (not= target "No server")
                                 (update! state side (assoc card :server-target target))))}]
     {:events {:runner-turn-begins ability
               :successful-run
               {:req (req (= (zone->name (get-in @state [:run :server])) (:server-target (get-card state card))))
                :once :per-turn
                :effect (req (let [st card]
                               (swap! state assoc-in [:run :run-effect :replace-access]
                                      {:mandatory true
                                       :effect (effect (resolve-ability
                                                         {:msg "gain 2 [Credits] instead of accessing"
                                                          :effect (effect (gain :credit 2)
                                                                          (update! (dissoc st :server-target)))}
                                                         st nil))})))}
               :runner-turn-ends {:effect (effect (update! (dissoc card :server-target)))}}
      :abilities [ability]})})
