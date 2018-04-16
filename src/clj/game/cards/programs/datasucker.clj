(in-ns 'game.core)

(declare can-host?)

(def card-programs-datasucker
  {"Datasucker"
   {:events (let [ds {:effect (req (update! state side (dissoc card :datasucker-count)))}]
              {:successful-run {:silent (req true)
                                :effect (effect (add-counter card :virus 1))
                                :req (req (#{:hq :rd :archives} target))}
               :pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice))
                                                 (:datasucker-count card)))
                                  :effect (req (let [c (:datasucker-count (get-card state card))]
                                                 (ice-strength-bonus state side (- c) target)))}
               :pass-ice ds :run-ends ds})
    :abilities [{:counter-cost [:virus 1]
                 :msg (msg "give -1 strength to " (:title current-ice))
                 :req (req (and current-ice (:rezzed current-ice)))
                 :effect (req (update! state side (update-in card [:datasucker-count] (fnil #(+ % 1) 0)))
                              (update-ice-strength state side current-ice))}]}})