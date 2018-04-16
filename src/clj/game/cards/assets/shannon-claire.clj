(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-shannon-claire
  {"Shannon Claire"
   {:abilities [{:cost [:click 1]
                 :msg "draw 1 card from the bottom of R&D"
                 :effect (effect (move (last (:deck corp)) :hand))}
                {:label "[Trash]: Search R&D for an agenda"
                 :prompt "Choose an agenda to add to the bottom of R&D"
                 :msg (msg "reveal " (:title target) " from R&D and add it to the bottom of R&D")
                 :choices (req (cancellable (filter #(is-type? % "Agenda") (:deck corp)) :sorted))
                 :effect (effect (shuffle! :deck) (move target :deck)
                                 (trash card {:cause :ability-cost}))}
                {:label "[Trash]: Search Archives for an agenda"
                 :prompt "Choose an agenda to add to the bottom of R&D"
                 :msg (msg "reveal " (:title target) " from Archives and add it to the bottom of R&D")
                 :choices (req (cancellable (filter #(is-type? % "Agenda") (:discard corp)) :sorted))
                 :effect (effect (move target :deck) (trash card {:cause :ability-cost}))}]}})
