(in-ns 'game.core)

(def card-definitions-identities-null-whistleblower
  {"Null: Whistleblower"
   {:abilities [{:once :per-turn
                 :req (req (and (:run @state) (rezzed? current-ice)))
                 :prompt "Select a card in your Grip to trash"
                 :choices {:req in-hand?}
                 :msg (msg "trash " (:title target) " and reduce the strength of " (:title current-ice)
                           " by 2 for the remainder of the run")
                 :effect (effect (update! (assoc card :null-target current-ice))
                                 (update-ice-strength current-ice)
                                 (trash target {:unpreventable true}))}]
    :events {:pre-ice-strength
             {:req (req (= (:cid target) (get-in card [:null-target :cid])))
              :effect (effect (ice-strength-bonus -2 target))}
             :run-ends
             {:effect (req (swap! state dissoc-in [:runner :identity :null-target]))}}}})
