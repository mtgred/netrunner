(in-ns 'game.cards.agendas)

(def card-definition-genetic-resequencing
  {"Genetic Resequencing"
   {:choices {:req #(= (last (:zone %)) :scored)}
    :msg (msg "add 1 agenda counter on " (:title target))
    :effect (effect (add-counter target :agenda 1))
    :silent (req true)}})
