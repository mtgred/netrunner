(in-ns 'game.cards.hardware)

(def card-definition-hijacked-router
  {"Hijacked Router"
   {:events {:server-created {:effect (effect (lose-credits :corp 1))
                              :msg "force the Corp to lose 1 [Credits]"}
             :successful-run {:req (req (= target :archives))
                              :optional {:prompt "Trash Hijacked Router to force the Corp to lose 3 [Credits]?"
                                         :yes-ability {:async true
                                                       :effect (req (system-msg state :runner "trashes Hijacked Router to force the Corp to lose 3 [Credits]")
                                                                    (wait-for (trash state :runner card {:unpreventable true})
                                                                              (lose-credits state :corp 3)
                                                                              (effect-completed state side eid)))}}}}}})
