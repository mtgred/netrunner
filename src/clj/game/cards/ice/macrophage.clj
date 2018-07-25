(in-ns 'game.cards.ice)

(def card-definition-macrophage
  {"Macrophage"
   {:subroutines [(trace-ability 4 {:label "Purge virus counters"
                                    :msg "purge virus counters"
                                    :effect (effect (purge))})
                  (trace-ability 3 {:label "Trash a virus"
                                    :prompt "Choose a virus to trash"
                                    :msg (msg "trash " (:title target))
                                    :choices {:req #(and (installed? %)
                                                         (has? % :subtype "Virus"))}
                                    :effect (effect (trash target {:cause :subroutine})
                                                    (clear-wait-prompt :runner))})
                  (trace-ability 2 {:label "Remove a virus in the Heap from the game"
                                    :prompt "Choose a virus in the Heap to remove from the game"
                                    :choices (req (cancellable (filter #(has? % :subtype "Virus") (:discard runner)) :sorted))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :runner target :rfg))})
                  (trace-ability 1 end-the-run)]}})
