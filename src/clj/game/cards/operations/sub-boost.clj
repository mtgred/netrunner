(in-ns 'game.core)

(def card-operations-sub-boost
  {"Sub Boost"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
    :effect (effect (update! (assoc target :subtype (combine-subtypes true (:subtype target) "Barrier")))
                    (update-ice-strength target)
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}})