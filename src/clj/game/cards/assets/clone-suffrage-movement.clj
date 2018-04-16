(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-clone-suffrage-movement
  {"Clone Suffrage Movement"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (and (some #(is-type? % "Operation") (:discard corp))
                                     unprotected))}
    :abilities [{:label "Add 1 operation from Archives to HQ"
                 :prompt "Select an operation in Archives to add to HQ" :show-discard true
                 :choices {:req #(and (is-type? % "Operation")
                                      (= (:zone %) [:discard]))}
                 :effect (effect (move target :hand)) :once :per-turn
                 :msg (msg "add " (if (:seen target) (:title target) "a facedown card") " to HQ")}]}})