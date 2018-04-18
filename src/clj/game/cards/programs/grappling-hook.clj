(in-ns 'game.core)

(def card-definitions-programs-grappling-hook
  {"Grappling Hook"
   {:abilities [{:msg "break all but 1 subroutine" :effect (effect (trash card {:cause :ability-cost}))}]}})
