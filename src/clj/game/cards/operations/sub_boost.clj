(in-ns 'game.cards.operations)

(def card-definition-sub-boost
  {"Sub Boost"
   (let [new-sub {:label "[Sub Boost]: End the run"}]
     {:sub-effect {:label "End the run"
                   :msg "end the run"
                   :effect (effect (end-run))}
      :choices {:req #(and (ice? %) (rezzed? %))}
      :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
      :effect (req (update! state side (assoc target :subtype (combine-subtypes true (:subtype target) "Barrier")))
                      (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub)
                      (update-ice-strength state side target)
                      (host state side (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
      :leave-play (req (remove-extra-subs state :corp (:cid card) (:host card)))
      :events {:rez {:req (req (= (:cid target) (:cid (:host card))))
                     :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub))}}})})
