(in-ns 'game.cards.resources)

(def card-definition-same-old-thing
  {"Same Old Thing"
   {:abilities [{:cost [:click 2]
                 :req (req (and (not (seq (get-in @state [:runner :locked :discard])))
                                (pos? (count (filter #(is-type? % "Event") (:discard runner))))))
                 :label "Play event from heap"
                 :prompt "Select an event to play"
                 :msg (msg "play " (:title target))
                 :show-discard true
                 :choices {:req #(and (is-type? % "Event")
                                      (= (:zone %) [:discard]))}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (play-instant target))}]}})
