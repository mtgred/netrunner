(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-lily-lockwell
  {"Lily Lockwell"
   {:delayed-completion true
    :effect (effect (draw eid 3 nil))
    :msg (msg "draw 3 cards")
    :abilities [{:label "Remove a tag to search R&D for an operation"
                 :prompt "Choose an operation to put on top of R&D"
                 :cost [:click 1]
                 :choices (req (let [ops (filter #(is-type? % "Operation") (:deck corp))]
                                 (if (empty? ops) ["No Operation in R&D"] ops)))
                 :req (req (pos? (get-in @state [:runner :tag])))
                 :effect (req (if (not= target "No Operation found")
                                (let [c (move state :corp target :play-area)]
                                  (shuffle! state :corp :deck)
                                  (move state :corp c :deck {:front true})
                                  (system-msg state side (str "uses Lily Lockwell to put " (:title c) " on top of R&D")))
                                (do (shuffle! state :corp :deck)
                                    (system-msg state side (str "uses Lily Lockwell, but did not find an Operation in R&D"))))
                              (lose state :runner :tag 1))}]}})