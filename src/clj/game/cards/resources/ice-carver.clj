(in-ns 'game.core)

(def card-definitions-resources-ice-carver
  {"Ice Carver"
   {:events {:pre-ice-strength
             {:req (req (and (= (:cid target) (:cid current-ice)) (:rezzed target)))
              :effect (effect (ice-strength-bonus -1 target))}}}})
