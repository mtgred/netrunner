(in-ns 'game.core)

(def card-definitions-assets-full-immersion-recstudio
  {"Full Immersion RecStudio"
   {:can-host (req (and (or (is-type? target "Asset") (is-type? target "Agenda"))
                        (> 2 (count (:hosted card)))))
    :trash-cost-bonus (req (* 3 (count (:hosted card))))
    :abilities [{:label "Install an asset or agenda on Full Immersion RecStudio"
                 :req (req (< (count (:hosted card)) 2))
                 :cost [:click 1]
                 :prompt "Select an asset or agenda to install"
                 :choices {:req #(and (or (is-type? % "Asset") (is-type? % "Agenda"))
                                      (in-hand? %)
                                      (= (:side %) "Corp"))}
                 :msg "install and host an asset or agenda"
                 :effect (req (corp-install state side target card))}
                {:label "Install a previously-installed asset or agenda on Full Immersion RecStudio (fixes only)"
                 :req (req (< (count (:hosted card)) 2))
                 :prompt "Select an installed asset or agenda to host on Full Immersion RecStudio"
                 :choices {:req #(and (or (is-type? % "Asset") (is-type? % "Agenda"))
                                      (installed? %)
                                      (= (:side %) "Corp"))}
                 :msg "install and host an asset or agenda"
                 :effect (req (host state side card target))}]}})
