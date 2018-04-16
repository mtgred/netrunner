(in-ns 'game.core)

(declare can-host?)

(def card-programs-keyhole
  {"Keyhole"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on R&D"
                 :makes-run true
                 :effect (effect (run :rd
                                   {:req (req (= target :rd))
                                    :replace-access
                                    {:prompt "Choose a card to trash"
                                     :not-distinct true
                                     :msg (msg "trash " (:title target))
                                     :choices (req (take 3 (:deck corp)))
                                     :mandatory true
                                     :effect (effect (trash (assoc target :seen true))
                                                     (shuffle! :corp :deck))}} card))}]}})
