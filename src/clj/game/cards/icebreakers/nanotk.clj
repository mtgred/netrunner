(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-nanotk
  {"NaNotK"
   (auto-icebreaker ["Sentry"]
                    {:effect (req (add-watch state (keyword (str "nanotk" (:cid card)))
                                              (fn [k ref old new]
                                                (let [server (first (get-in @state [:run :server]))]
                                                  (when (or
                                                          ; run initiated or ended
                                                          (not= (get-in old [:run])
                                                                (get-in new [:run]))
                                                          ; server configuration changed (redirected or newly installed ICE)
                                                          (not= (get-in old [:corp :servers server :ices])
                                                                (get-in new [:corp :servers server :ices])))
                                                    (update-breaker-strength ref side card))))))
                     :strength-bonus (req (if-let [numice (count run-ices)] numice 0))
                     :leave-play (req (remove-watch state (keyword (str "nanotk" (:cid card)))))
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 2)]})})
