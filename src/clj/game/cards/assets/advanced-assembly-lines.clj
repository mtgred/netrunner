(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-advanced-assembly-lines
  {"Advanced Assembly Lines"
   {:effect (effect (gain :credit 3))
    :msg (msg "gain 3 [Credits]")
    :abilities [{:label "[Trash]: Install a non-agenda card from HQ"
                 :effect (effect (trash card) (corp-install target nil))
                 :msg (msg (corp-install-msg target))
                 :prompt "Select a non-agenda card to install from HQ"
                 :priority true
                 :req (req (not (:run @state)))
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (not (is-type? % "Agenda"))
                                      (= (:zone %) [:hand])
                                      (= (:side %) "Corp"))}}]}})