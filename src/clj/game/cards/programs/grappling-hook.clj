(in-ns 'game.core)

(declare can-host?)

(def card-programs-grappling-hook
  {"Grappling Hook"
   {:abilities [{:msg "break all but 1 subroutine" :effect (effect (trash card {:cause :ability-cost}))}]}})