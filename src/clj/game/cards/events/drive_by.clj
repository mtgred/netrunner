(in-ns 'game.cards.events)

(def card-definition-drive-by
  {"Drive By"
   {:choices {:req #(let [topmost (get-nested-host %)]
                     (and (is-remote? (second (:zone topmost)))
                          (= (last (:zone topmost)) :content)
                          (not (:rezzed %))))}
    :async true
    :effect (req (wait-for (expose state side target) ;; would be nice if this could return a value on completion
                           (if async-result ;; expose was successful
                             (if (#{"Asset" "Upgrade"} (:type target))
                               (do (system-msg state :runner (str "uses Drive By to trash " (:title target)))
                                   (trash state side (assoc target :seen true))
                                   ;; Turn on Reprisal cards
                                   (swap! state assoc-in [:runner :register :trashed-card] true)
                                   (effect-completed state side eid))
                               (effect-completed state side eid))
                             (effect-completed state side eid))))}})
