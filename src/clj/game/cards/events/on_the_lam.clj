(in-ns 'game.cards.events)

(def card-definition-on-the-lam
  {"On the Lam"
   {:req (req (some #(is-type? % "Resource") (all-active-installed state :runner)))
    :prompt "Choose a resource to host On the Lam"
    :choices {:req #(and (is-type? % "Resource")
                         (installed? %))}
    :effect (effect (host target (assoc card :zone [:discard] :installed true))
                    (system-msg (str "hosts On the Lam on " (:title target))))
    :interactions {:prevent [{:type #{:net :brain :meat :tag}
                              :req (req true)}]}
    :abilities [{:label "[Trash]: Avoid 3 tags"
                 :msg "avoid up to 3 tags"
                 :effect (effect (tag-prevent :runner 3)
                                 (trash card {:cause :ability-cost}))}
                {:label "[Trash]: Prevent up to 3 damage"
                 :msg "prevent up to 3 damage"
                 :effect (effect (damage-prevent :net 3)
                                 (damage-prevent :meat 3)
                                 (damage-prevent :brain 3)
                                 (trash card {:cause :ability-cost}))}]}})
