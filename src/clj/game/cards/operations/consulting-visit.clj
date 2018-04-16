(in-ns 'game.core)

(def card-operations-consulting-visit
  {"Consulting Visit"
   {:prompt  "Choose an Operation from R&D to play"
    :choices (req (cancellable
             (filter #(and (is-type? % "Operation")
                           (<= (:cost %) (:credit corp)))
                      (:deck corp))
             :sorted))
    :effect  (effect (shuffle! :deck)
                     (system-msg "shuffles their deck")
                     (play-instant target))
    :msg (msg "search R&D for " (:title target) " and play it")}})