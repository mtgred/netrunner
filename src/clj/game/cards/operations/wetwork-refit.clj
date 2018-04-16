(in-ns 'game.core)

(def card-operations-wetwork-refit
  {"Wetwork Refit"
   {:choices {:req #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (rezzed? %))}
    :msg (msg "give " (card-str state target) "\"[Subroutine] Do 1 brain damage\" before all its other subroutines")
    :effect (effect (update! (assoc target :subroutines (cons (do-brain-damage 1) (:subroutines target))))
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
    :leave-play (effect (update! (assoc (:host card) :subroutines (rest (:subroutines (:host card))))))}})