(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-laramy-fisk-savvy-investor
  {"Laramy Fisk: Savvy Investor"
   {:events
    {:successful-run
     {:delayed-completion true
      :interactive (req true)
      :req (req (and (is-central? (:server run))
                     (empty? (let [successes (turn-events state side :successful-run)]
                               (filter #(is-central? %) successes)))))
      :effect (effect (continue-ability
                        {:optional
                         {:prompt "Force the Corp to draw a card?"
                          :yes-ability {:msg "force the Corp to draw 1 card"
                                        :effect (effect (draw :corp))}
                          :no-ability {:effect (effect (system-msg "declines to use Laramy Fisk: Savvy Investor"))}}}
                        card nil))}}}})