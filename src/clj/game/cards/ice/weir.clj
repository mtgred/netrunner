(in-ns 'game.cards.ice)

(def card-definition-weir
  {"Weir"
   {:subroutines [{:label "force the Runner to lose 1 [Click], if able"
                   :msg "force the Runner to lose 1 [Click]"
                   :effect runner-loses-click}
                  {:label "Runner trashes 1 card from their Grip"
                   :req (req (pos? (count (:hand runner))))
                   :prompt "Choose a card to trash from your Grip"
                   :player :runner
                   :choices (req (:hand runner))
                   :not-distinct true
                   :effect (effect (trash :runner target)
                                   (system-msg :runner (str "trashes " (:title target) " from their Grip")))}]}})
