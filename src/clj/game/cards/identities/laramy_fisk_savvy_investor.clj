(in-ns 'game.cards.identities)

(def card-definition-laramy-fisk-savvy-investor
  {"Laramy Fisk: Savvy Investor"
   {:events
    {:successful-run
     {:async true
      :interactive (req true)
      :req (req (and (is-central? (:server run))
                     (first-event? state side :successful-run is-central?)))
      :effect (effect (continue-ability
                        {:optional
                         {:prompt "Force the Corp to draw a card?"
                          :yes-ability {:msg "force the Corp to draw 1 card"
                                        :async true
                                        :effect (effect (draw :corp eid 1 nil))}
                          :no-ability {:effect (effect (system-msg "declines to use Laramy Fisk: Savvy Investor"))}}}
                        card nil))}}}})
