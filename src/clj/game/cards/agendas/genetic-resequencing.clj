(in-ns 'game.core)

(def card-definitions-agendas-genetic-resequencing
  {"Genetic Resequencing"
   {:choices {:req #(= (last (:zone %)) :scored)}
    :msg (msg "add 1 agenda counter on " (:title target))
    :effect (final-effect (add-counter target :agenda 1))
    :silent (req true)}})
