(in-ns 'game.cards.resources)

(def card-definition-reclaim
  {"Reclaim"
   {:abilities
    [{:label "Install a program, piece of hardware, or virtual resource from your Heap"
      :cost [:click 1]
      :req (req (not-empty (:hand runner)))
      :prompt "Choose a card to trash"
      :choices (req (cancellable (:hand runner) :sorted))
      :async true
      :effect (req (wait-for
                     (trash state :runner card {:cause :ability-cost})
                     (wait-for
                       (trash state :runner target {:unpreventable true})
                       (continue-ability
                         state :runner
                         {:prompt "Choose a card to install"
                          :choices (req (cancellable
                                          (filter #(and (or (is-type? % "Program")
                                                            (is-type? % "Hardware")
                                                            (and (is-type? % "Resource")
                                                                 (has-subtype? % "Virtual")))
                                                        (can-pay? state :runner nil (:cost %)))
                                                  (:discard runner))
                                          :sorted))
                          :msg (msg "install " (:title target) " from the Heap")
                          :async true
                          :effect (req (runner-install state :runner eid target nil))}
                         card nil))))}]}})
