(in-ns 'game.cards.programs)

(def card-definition-grappling-hook
  {"Grappling Hook"
   {:abilities [{:msg "break all but 1 subroutine" :effect (effect (trash card {:cause :ability-cost}))}]}})
