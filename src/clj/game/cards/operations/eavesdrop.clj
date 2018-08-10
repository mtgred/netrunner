(in-ns 'game.cards.operations)

(def card-definition-eavesdrop
  {"Eavesdrop"
   (let [new-sub {:label "[Eavesdrop]: Trace 3 - Give the Runner 1 tag"}]
     {:implementation "On encounter effect is manual"
      :sub-effect {:label "Give the Runner 1 tag"
                   :trace {:base 3
                           :successful {:msg "give the Runner 1 tag"
                                        :async true
                                        :effect (effect (gain-tags :runner eid 1))}}}
      :choices {:req #(and (ice? %)
                           (installed? %))}
      :msg (msg "give " (card-str state target {:visible false}) " additional text")
      :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub)
                   (update-ice-strength state side target)
                   (host state side (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
      :leave-play (req (remove-extra-subs state :corp (:cid card) (:host card)))
      :events {:rez {:req (req (= (:cid target) (:cid (:host card))))
                     :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub))}}})})
