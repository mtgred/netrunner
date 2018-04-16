(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-watchdog
  {"Watchdog"
   {:events {:pre-rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                       :effect (effect (rez-cost-bonus (- (:tag runner))))}
             :rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}})