(in-ns 'game.cards.events)

(def card-definition-reboot
  {"Reboot"
   (letfn [(install-cards [state side eid card to-install titles]
             (if-let [f (first to-install)]
               (wait-for (runner-install state :runner f {:facedown true :no-msg true})
                         (install-cards state side eid card (rest to-install) titles))
               (do
                 (move state side (find-latest state card) :rfg)
                 (system-msg state :runner (str "uses Reboot to install " (join ", " titles) " facedown"))
                 (effect-completed state side eid))))]
     {:req (req archives-runnable)
      :makes-run true
      :effect (effect
                (run :archives
                     {:req (req (= target :archives))
                      :replace-access
                      {:prompt "Choose up to five cards to install"
                       :choices {:max 5
                                 :req #(and (in-discard? %) (= (:side %) "Runner") (not= (:cid %) (:cid card)))}
                       :mandatory true
                       :async true
                       :cancel-effect (req (move state side (find-latest state card) :rfg)
                                           (effect-completed state side eid))
                       :effect (req (install-cards state side eid card targets (map :title targets)))}}
                     card))})})
