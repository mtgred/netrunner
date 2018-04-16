(in-ns 'game.core)

(def card-operations-hunter-seeker
  {"Hunter Seeker"
   {:req (req (last-turn? state :runner :stole-agenda))
    :delayed-completion true
    :prompt "Choose a card to trash"
    :choices {:req installed?}
    :msg (msg "trash " (card-str state target))
    :effect (effect (trash target))}})