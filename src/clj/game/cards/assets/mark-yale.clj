(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-mark-yale
  {"Mark Yale"
   {:events {:agenda-counter-spent {:effect (effect (gain :credit 1))
                                    :msg "gain 1 [Credits]"}}
    :abilities [{:label "Trash to gain 2 [Credits]"
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain :credit 2) (trash card))}
                {:label "Spend an agenda counter to gain 2 [Credits]"
                 :effect (req (resolve-ability
                                state side
                                {:prompt "Select an agenda with a counter"
                                 :choices {:req #(and (is-type? % "Agenda")
                                                      (pos? (get-in % [:counter :agenda] 0)))}
                                 :effect (req (add-counter state side target :agenda -1)
                                              (gain state :corp :credit 2)
                                              (trigger-event state side :agenda-counter-spent card))
                                 :msg (msg "spend an agenda token on " (:title target) " and gain 2 [Credits]")}
                                card nil))}]}})