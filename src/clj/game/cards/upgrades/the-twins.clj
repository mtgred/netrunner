(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-the-twins
  {"The Twins"
   {:abilities [{:label "Reveal and trash a copy of the ICE just passed from HQ"
                 :req (req (and this-server
                                (> (count (get-run-ices state)) (:position run))
                                (:rezzed (get-in (:ices (card->server state card)) [(:position run)]))))
                 :effect (req (let [icename (:title (get-in (:ices (card->server state card)) [(:position run)]))]
                                (resolve-ability
                                  state side
                                  {:prompt "Select a copy of the ICE just passed"
                                   :choices {:req #(and (in-hand? %)
                                                        (ice? %)
                                                        (= (:title %) icename))}
                                   :effect (req (trash state side (assoc target :seen true))
                                                (swap! state update-in [:run]
                                                       #(assoc % :position (inc (:position run)))))
                                   :msg (msg "trash a copy of " (:title target) " from HQ and force the Runner to encounter it again")}
                                 card nil)))}]}})
