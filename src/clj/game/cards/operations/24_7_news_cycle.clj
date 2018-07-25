(in-ns 'game.cards.operations)

(def card-definition-24-7-news-cycle
  {"24/7 News Cycle"
   {:req (req (pos? (count (:scored corp))))
    :async true
    :additional-cost [:forfeit]
    :effect (req (continue-ability
                   state side
                   {:prompt "Select an agenda in your score area to trigger its \"when scored\" ability"
                    :choices {:req #(and (is-type? % "Agenda")
                                         (when-scored? %)
                                         (is-scored? state :corp %))}
                    :msg (msg "trigger the \"when scored\" ability of " (:title target))
                    :async true
                    ;dissoc :end-turn for Breaking News
                    :effect (effect (continue-ability (dissoc (card-def target) :end-turn) target nil))}
                   card nil))}})
