(in-ns 'game.core)

(def card-definitions-icebreakers-adept
  {"Adept"
   {:abilities [{:cost [:credit 2] :req (req (or (has-subtype? current-ice "Barrier")
                                                 (has-subtype? current-ice "Sentry")))
                 :msg "break 1 Sentry or Barrier subroutine"}]
    :effect (req (add-watch state (keyword (str "adept" (:cid card)))
                            (fn [k ref old new]
                              (when (not= (get-in old [:runner :memory]) (get-in new [:runner :memory]))
                                (update-breaker-strength ref side card))))
                 (update-breaker-strength state side card))
    :leave-play (req (remove-watch state (keyword (str "adept" (:cid card)))))
    :strength-bonus (req (:memory runner))}})
