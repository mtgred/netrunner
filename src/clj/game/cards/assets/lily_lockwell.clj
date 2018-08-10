(in-ns 'game.cards.assets)

(def card-definition-lily-lockwell
  {"Lily Lockwell"
   {:async true
    :effect (effect (draw eid 3 nil))
    :msg (msg "draw 3 cards")
    :abilities [{:label "Remove a tag to search R&D for an operation"
                 :prompt "Choose an operation to put on top of R&D"
                 :cost [:click 1]
                 :choices (req (cancellable (filter #(is-type? % "Operation") (:deck corp)) :sorted))
                 :req (req (pos? (get-in @state [:runner :tag])))
                 :effect (req (lose-tags state :corp 1)
                              (let [c (move state :corp target :play-area)]
                                (shuffle! state :corp :deck)
                                (move state :corp c :deck {:front true})
                                (system-msg state side (str "uses Lily Lockwell to put " (:title c) " on top of R&D"))))
                 :cancel-effect (effect (lose-tags :corp 1)
                                        (shuffle! :corp :deck)
                                        (system-msg (str "uses Lily Lockwell, but did not find an Operation in R&D")))}]}})
