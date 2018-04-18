(in-ns 'game.core)

(def card-definitions-assets-tech-startup
  {"Tech Startup"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :abilities [{:label "Install an asset from R&D"
                 :prompt "Choose an asset to install"
                 :msg (msg "install " (:title target))
                 :choices (req (filter #(is-type? % "Asset") (:deck corp)))
                 :effect (effect (trash card)
                                 (shuffle! :deck)
                                 (corp-install target nil))}]}})
