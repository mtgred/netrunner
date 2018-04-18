(in-ns 'game.core)

(def card-definitions-resources-the-shadow-net
  {"The Shadow Net"
   (letfn [(events [runner] (filter #(and (is-type? % "Event") (not (has-subtype? % "Priority"))) (:discard runner)))]
     {:abilities [{:cost [:click 1 :forfeit]
                   :req (req (< 0 (count (events runner))))
                   :label "Play an event from your Heap, ignoring all costs"
                   :prompt "Choose an event to play"
                   :msg (msg "play " (:title target) " from the Heap, ignoring all costs")
                   :choices (req (cancellable (events runner) :sorted))
                   :effect (effect (play-instant nil target {:ignore-cost true}))}]})})
