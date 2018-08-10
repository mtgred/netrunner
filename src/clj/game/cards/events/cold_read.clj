(in-ns 'game.cards.events)

(def card-definition-cold-read
  {"Cold Read"
   (let [end-effect {:prompt "Choose a program that was used during the run to trash "
                     :choices {:req #(card-is? % :type "Program")}
                     :msg (msg "trash " (:title target))
                     :effect (effect (trash target {:unpreventable true}))}]
     {:async true
      :prompt "Choose a server"
      :recurring 4
      :choices (req runnable-servers)
      :effect (req (let [c (move state side (assoc card :zone '(:discard)) :play-area {:force true})]
                     (card-init state side c {:resolve-effect false})
                     (game.core/run state side (make-eid state) target
                                    {:end-run {:async true
                                               :effect (effect (trash c)
                                                               (continue-ability end-effect card nil))}}
                                    c)))})})
