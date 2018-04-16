(in-ns 'game.core)

(def card-operations-mca-informant
  {"MCA Informant"
   {:implementation "Runner must deduct 1 click and 2 credits, then trash host manually"
    :req (req (not-empty (filter #(has-subtype? % "Connection") (all-active-installed state :runner))))
    :prompt "Choose a connection to host MCA Informant on it"
    :choices {:req #(and (= (:side %) "Runner") (has-subtype? % "Connection") (installed? %))}
    :msg (msg "host it on " (card-str state target) ". The Runner has an additional tag")
    :effect (req (host state side (get-card state target) (assoc card :zone [:discard] :seen true))
                 (swap! state update-in [:runner :tag] inc))
    :leave-play (req (swap! state update-in [:runner :tag] dec)
                     (system-msg state :corp "trashes MCA Informant"))}})
