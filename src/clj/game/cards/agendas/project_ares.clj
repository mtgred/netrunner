(in-ns 'game.cards.agendas)

(def card-definition-project-ares
  {"Project Ares"
   (letfn [(trash-count-str [card]
             (quantify (- (get-counters card :advancement) 4) "installed card"))]
     {:silent (req true)
      :req (req (and (> (get-counters card :advancement) 4)
                     (pos? (count (all-installed state :runner)))))
      :msg (msg "force the Runner to trash " (trash-count-str card) " and take 1 bad publicity")
      :async true
      :effect (effect (show-wait-prompt :corp "Runner to trash installed cards")
                      (continue-ability
                       :runner
                       {:prompt (msg "Select " (trash-count-str card) " installed cards to trash")
                        :choices {:max (min (- (get-counters card :advancement) 4)
                                            (count (all-installed state :runner)))
                                  :req #(and (= (:side %) "Runner")
                                             (:installed %))}
                        :effect (effect (trash-cards targets)
                                        (system-msg (str "trashes " (join ", " (map :title targets))))
                                        (gain-bad-publicity :corp 1))}
                       card nil)
                      (clear-wait-prompt :corp))})})
