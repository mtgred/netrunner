(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-project-ares
  {"Project Ares"
   (letfn [(trash-count-str [card]
             (quantify (- (:advance-counter card) 4) "installed card"))]
     {:silent (req true)
      :req (req (and (> (:advance-counter card) 4)
                     (pos? (count (all-installed state :runner)))))
      :msg (msg "force the Runner to trash " (trash-count-str card) " and take 1 bad publicity")
      :delayed-completion true
      :effect (effect (show-wait-prompt :corp "Runner to trash installed cards")
                      (continue-ability
                       :runner
                       {:prompt (msg "Select " (trash-count-str card) " installed cards to trash")
                        :choices {:max (min (- (:advance-counter card) 4)
                                            (count (all-installed state :runner)))
                                  :req #(and (= (:side %) "Runner")
                                             (:installed %))}
                        :effect (final-effect (trash-cards targets)
                                              (system-msg (str "trashes " (join ", " (map :title targets))))
                                              (gain-bad-publicity :corp 1))}
                       card nil)
                      (clear-wait-prompt :corp))})})
