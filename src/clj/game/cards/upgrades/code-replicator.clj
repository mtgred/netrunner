(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-code-replicator
  {"Code Replicator"
   {:abilities [{:label "[Trash]: Force the runner to approach the passed piece of ice again"
                 :req (req (and this-server
                                (> (count (get-run-ices state)) (:position run))
                                (:rezzed (get-in (:ices (card->server state card)) [(:position run)]))))
                 :effect (req (let [icename (:title (get-in (:ices (card->server state card)) [(:position run)]))]
                                (trash state :corp (get-card state card))
                                (swap! state update-in [:run] #(assoc % :position (inc (:position run))))
                                 (system-msg state :corp (str "trashes Code Replicator to make the runner approach "
                                                              icename " again"))))}]}})