(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-savant
  {"Savant"
   {:abilities [{:cost [:credit 2] :req (req (has-subtype? current-ice "Sentry"))
                 :msg "break 1 Sentry subroutine"}
                {:cost [:credit 2] :req (req (has-subtype? current-ice "Code Gate"))
                              :msg "break 2 Code Gate subroutines"}]
    :effect (req (add-watch state (keyword (str "savant" (:cid card)))
                            (fn [k ref old new]
                              (when (not= (get-in old [:runner :memory]) (get-in new [:runner :memory]))
                                (update-breaker-strength ref side card))))
                 (update-breaker-strength state side card))
    :leave-play (req (remove-watch state (keyword (str "savant" (:cid card)))))
    :strength-bonus (req (:memory runner))}})