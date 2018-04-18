(in-ns 'game.core)

(def card-definitions-operations-oversight-ai
  {"Oversight AI"
   {:implementation "Trashing ICE is manual"
    :choices {:req #(and (ice? %) (not (rezzed? %)) (= (last (:zone %)) :ices))}
    :msg (msg "rez " (:title target) " at no cost")
    :effect (final-effect (rez target {:ignore-cost :all-costs})
                          (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}})
