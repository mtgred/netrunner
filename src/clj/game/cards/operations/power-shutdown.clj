(in-ns 'game.core)

(def card-operations-power-shutdown
  {"Power Shutdown"
   {:req (req (last-turn? state :runner :made-run))
    :prompt "Trash how many cards from the top R&D?"
    :choices {:number (req (apply max (map :cost (filter #(or (= "Program" (:type %)) (= "Hardware" (:type %))) (all-active-installed state :runner)))))}
    :msg (msg "trash " target " cards from the top of R&D")
    :delayed-completion true
    :effect (req (mill state :corp target)
                 (let [n target]
                   (continue-ability state :runner
                                     {:prompt "Select a Program or piece of Hardware to trash"
                                      :choices {:req #(and (#{"Hardware" "Program"} (:type %))
                                                           (<= (:cost %) n))}
                                      :msg (msg "trash " (:title target))
                                      :effect (effect (trash target))}
                                    card nil)))}})