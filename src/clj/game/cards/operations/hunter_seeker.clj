(in-ns 'game.cards.operations)

(def card-definition-hunter-seeker
  {"Hunter Seeker"
   {:req (req (last-turn? state :runner :stole-agenda))
    :async true
    :prompt "Choose a card to trash"
    :choices {:req installed?}
    :msg (msg "trash " (card-str state target))
    :effect (effect (trash target))}})
