(in-ns 'game.cards.events)

(def card-definition-corporate-grant
  {"Corporate \"Grant\""
   {:events {:runner-install {:silent (req true) ;; there are no current interactions where we'd want Grant to not be last, and this fixes a bug with Hayley
                              :req (req (first-event? state side :runner-install))
                              :msg "force the Corp to lose 1 [Credit]"
                              :effect (effect (lose-credits :corp 1))}}}})
