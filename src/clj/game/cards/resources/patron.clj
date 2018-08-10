(in-ns 'game.cards.resources)

(def card-definition-patron
  {"Patron"
   (let [ability {:label "Choose a server"
                  :prompt "Choose a server for Patron"
                  :choices (req (conj servers "No server"))
                  :req (req (and (not (click-spent? :runner state))
                                 (not (used-this-turn? (:cid card) state))))
                  :msg (msg "target " target)
                  :effect (req (when (not= target "No server")
                                 (update! state side (assoc card :server-target target))))}]
     {:events {:runner-turn-begins ability
               :successful-run
               {:req (req (= (zone->name (get-in @state [:run :server]))
                             (:server-target (get-card state card))))
                :once :per-turn
                :effect (req (let [st card]
                               (swap! state assoc-in [:run :run-effect :replace-access]
                                      {:mandatory true
                                       :effect (effect
                                                 (resolve-ability
                                                   {:msg "draw 2 cards instead of accessing"
                                                    :effect (effect (draw 2)
                                                                    (update! (dissoc st :server-target)))}
                                                   st nil))})))}
               :runner-turn-ends {:effect (effect (update! (dissoc card :server-target)))}}
      :abilities [ability]})})
