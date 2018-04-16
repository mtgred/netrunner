(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-same-old-thing
  {"Same Old Thing"
   {:abilities [{:cost [:click 2]
                 :req (req (and (not (seq (get-in @state [:runner :locked :discard])))
                                (< 0 (count (filter #(is-type? % "Event") (:discard runner))))))
                 :prompt "Select an event to play"
                 :msg (msg "play " (:title target))
                 :show-discard true
                 :choices {:req #(and (is-type? % "Event")
                                      (= (:zone %) [:discard]))}
                 :effect (effect (trash card {:cause :ability-cost}) (play-instant target))}]}})