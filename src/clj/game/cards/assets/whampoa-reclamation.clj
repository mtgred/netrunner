(in-ns 'game.core)

(def card-definitions-assets-whampoa-reclamation
  {"Whampoa Reclamation"
   {:abilities [{:label "Trash 1 card from HQ: Add 1 card from Archives to the bottom of R&D"
                 :once :per-turn
                 :req (req (and (pos? (count (:hand corp)))
                                (pos? (count (:discard corp)))))
                 :delayed-completion true
                 :effect (req (show-wait-prompt state :runner "Corp to use Whampoa Reclamation")
                              (when-completed (resolve-ability state side
                                                {:prompt "Choose a card in HQ to trash"
                                                 :choices {:req #(and (in-hand? %) (= (:side %) "Corp"))}
                                                 :effect (effect (trash target))}
                                               card nil)
                                              (continue-ability state side
                                                {:prompt "Select a card in Archives to add to the bottom of R&D"
                                                 :show-discard true
                                                 :choices {:req #(and (in-discard? %) (= (:side %) "Corp"))}
                                                 :msg (msg "trash 1 card from HQ and add "
                                                           (if (:seen target) (:title target) "a card") " from Archives to the bottom of R&D")
                                                 :effect (effect (move target :deck)
                                                                 (clear-wait-prompt :runner))}
                                               card nil)))}]}})
