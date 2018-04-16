(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-wyrm
  {"Wyrm"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 3]
                                  :msg "break 1 subroutine on ICE with 0 or less strength"}
                                 {:cost [:credit 1]
                                  :label "Give -1 strength to current ICE"
                                  :req (req (rezzed? current-ice))
                                  :msg (msg "give -1 strength to " (:title current-ice))
                                  :effect (req (update! state side (update-in card [:wyrm-count] (fnil #(+ % 1) 0)))
                                               (update-ice-strength state side current-ice))}
                                 (strength-pump 1 1)]
                     :events (let [auto-pump (fn [state side eid card targets]
                                               ((:effect breaker-auto-pump) state side eid card targets))
                                   wy {:effect (effect (update! (dissoc card :wyrm-count))
                                                       (auto-pump eid (get-card state card) targets))}]
                               {:pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice))
                                                                  (:wyrm-count card)))
                                                   :effect (req (let [c (:wyrm-count (get-card state card))]
                                                                  (ice-strength-bonus state side (- c) target)
                                                                  (auto-pump state side eid card targets)))}
                                :pass-ice wy
                                :run-ends wy})})})
