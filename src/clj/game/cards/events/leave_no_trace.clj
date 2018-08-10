(in-ns 'game.cards.events)

(def card-definition-leave-no-trace
  {"Leave No Trace"
   (letfn [(get-rezzed-cids [ice]
             (map :cid (filter #(and (rezzed? %) (is-type? % "ICE")) ice)))]
     {:prompt "Choose a server"
      :msg "make a run and derez any ICE that are rezzed during this run"
      :choices (req runnable-servers)
      :async true
      :effect (req
                (let [old-ice-cids (get-rezzed-cids (all-installed state :corp))]
                  (swap! state assoc :lnt old-ice-cids)
                  (register-events state side (:events (card-def card)) (assoc card :zone '(:discard)))
                  (game.core/run state side (make-eid state) target nil card)))
      :events {:run-ends {:effect (req (let [new (set (get-rezzed-cids (all-installed state :corp)))
                                             old (set (:lnt @state))
                                             diff-cid (seq (clojure.set/difference new old))
                                             diff (map #(find-cid % (all-installed state :corp)) diff-cid)]
                                         (doseq [ice diff]
                                           (derez state side ice))
                                         (when-not (empty? diff)
                                           (system-msg state side (str "derezzes " (join ", " (map :title diff)) " via Leave No Trace")))
                                         (swap! state dissoc :lnt)
                                         (unregister-events state side card)))}}})})
