(in-ns 'game.core)

(def card-definitions-resources-assimilator
  {"Assimilator"
   {:abilities [{:label "Turn a facedown card faceup"
                 :cost [:click 2]
                 :prompt "Select a facedown installed card"
                 :choices {:req #(and (facedown? %)
                                      (installed? %)
                                      (= "Runner" (:side %)))}
                 :effect (req (if (or (is-type? target "Event")
                                      (and (has-subtype? target "Console")
                                           (some #(has-subtype? % "Console") (all-active-installed state :runner))))
                                ;; Consoles and events are immediately unpreventably trashed.
                                (trash state side target {:unpreventable true})
                                ;; Other cards are moved to rig and have events wired.
                                (flip-faceup state side target)))
                 :msg (msg "turn " (:title target) " faceup")}]}})
