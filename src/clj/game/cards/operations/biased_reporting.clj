(in-ns 'game.cards.operations)

(def card-definition-biased-reporting
  {"Biased Reporting"
   (letfn [(num-installed [state t]
             (count (filter #(is-type? % t) (all-active-installed state :runner))))]
     {:async true
      :req (req (not-empty (all-active-installed state :runner)))
      :prompt "Choose a card type"
      :choices ["Resource" "Hardware" "Program"]
      :effect (req (let [t target
                         n (num-installed state t)]
                     (show-wait-prompt state :corp "Runner to choose cards to trash")
                     (wait-for
                       (resolve-ability
                         state :runner
                         {:prompt (msg "Choose any number of cards of type " t " to trash")
                          :choices {:max n
                                    :req #(and (installed? %) (is-type? % t))}
                          :effect (req (doseq [c targets]
                                         (trash state :runner c {:unpreventable true}))
                                       (gain-credits state :runner (count targets))
                                       (system-msg state :runner
                                                   (str "trashes "
                                                        (join ", " (map :title (sort-by :title targets)))
                                                        " and gains " (count targets) " [Credits]"))
                                       (effect-completed state side eid))}
                         card nil)
                       (clear-wait-prompt state :corp)
                       (let [n (* 2 (num-installed state t))]
                         (when (pos? n)
                           (gain-credits state :corp n)
                           (system-msg state :corp (str "uses Biased Reporting to gain " n " [Credits]")))
                         (effect-completed state side eid)))))})})
