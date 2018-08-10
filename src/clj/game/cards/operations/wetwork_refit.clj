(in-ns 'game.cards.operations)

(def card-definition-wetwork-refit
  {"Wetwork Refit"
   (let [new-sub {:label "[Wetwork Refit] Do 1 brain damage"}]
     {:choices {:req #(and (ice? %)
                           (has-subtype? % "Bioroid")
                           (rezzed? %))}
      :msg (msg "give " (card-str state target) " \"[Subroutine] Do 1 brain damage\" before all its other subroutines")
      :sub-effect (do-brain-damage 1)
      :effect (req (add-extra-sub state :corp (:cid card) target 0 new-sub)
                   (host state side (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
      :leave-play (req (remove-extra-subs state :corp (:cid card) (:host card)))
      :events {:rez {:req (req (= (:cid target) (:cid (:host card))))
                     :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) 0 new-sub))}}})})
