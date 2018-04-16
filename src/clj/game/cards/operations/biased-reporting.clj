(in-ns 'game.core)

(def card-operations-biased-reporting
  {"Biased Reporting"
   {:delayed-completion true
    :req (req (not-empty (all-active-installed state :runner)))
    :prompt "Choose a card type"
    :choices ["Resource" "Hardware" "Program"]
    :effect (req (let [t target
                       num (count (filter #(is-type? % t) (all-active-installed state :runner)))]
                   (show-wait-prompt state :corp "Runner to choose cards to trash")
                   (when-completed
                     (resolve-ability state :runner
                       {:prompt (msg "Choose any number of cards of type " t " to trash")
                        :choices {:max num :req #(and (installed? %) (is-type? % t))}
                        :cancel-effect (final-effect (clear-wait-prompt :corp))
                        :effect (req (doseq [c targets]
                                       (trash state :runner c {:unpreventable true}))
                                     (gain state :runner :credit (count targets))
                                     (system-msg state :runner (str "trashes " (join ", " (map :title (sort-by :title targets)))
                                                                    " and gains " (count targets) " [Credits]"))
                                     (clear-wait-prompt state :corp))}
                      card nil)
                     (do (let [n (* 2 (count (filter #(is-type? % t) (all-active-installed state :runner))))]
                           (when (pos? n)
                             (gain state :corp :credit n)
                             (system-msg state :corp (str "uses Biased Reporting to gain " n " [Credits]")))
                           (effect-completed state side eid))))))}})
