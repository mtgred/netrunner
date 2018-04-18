(in-ns 'game.core)

(def card-definitions-events-cold-read
  {"Cold Read"
   (let [end-effect {:prompt "Choose a program that was used during the run to trash "
                     :choices {:req #(card-is? % :type "Program")}
                     :msg (msg "trash " (:title target))
                     :effect (effect (trash target {:unpreventable true}))}]
     {:delayed-completion true
      :prompt "Choose a server"
      :recurring 4
      :choices (req runnable-servers)
      :effect (req (let [c (move state side (assoc card :zone '(:discard)) :play-area {:force true})]
                     (card-init state side c {:resolve-effect false})
                     (game.core/run state side (make-eid state) target
                                    {:end-run {:delayed-completion true
                                               :effect (effect (trash c)
                                                               (continue-ability end-effect card nil))}}
                                    c)))})

  "Corporate \"Grant\""
  {:events {:runner-install {:silent (req true) ;; there are no current interactions where we'd want Grant to not be last, and this fixes a bug with Hayley
                             :req (req (first-event? state side :runner-install))
                             :msg "force the Corp to lose 1 [Credit]"
                             :effect (effect (lose :corp :credit 1))}}}})
