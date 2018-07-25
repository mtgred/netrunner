(in-ns 'game.cards.assets)

(def card-definition-political-dealings
  {"Political Dealings"
   (letfn [(pdhelper [agendas n]
             {:optional
              {:prompt (msg "Reveal and install " (:title (nth agendas n)) "?")
               :yes-ability {:async true
                             :msg (msg "reveal " (:title (nth agendas n)))
                             :effect (req (wait-for (corp-install
                                                      state side (nth agendas n) nil
                                                      {:install-state
                                                       (:install-state
                                                         (card-def (nth agendas n))
                                                         :unrezzed)})
                                                    (if (< (inc n) (count agendas))
                                                      (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                                      (effect-completed state side eid))))}
               :no-ability {:async true
                            :effect (req (if (< (inc n) (count agendas))
                                           (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                           (effect-completed state side eid)))}}})]
     {:events
      {:corp-draw
       {:async true
        :req (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                        agendas (filter #(is-type? % "Agenda") drawn)]
                    (seq agendas)))
        :effect (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                           agendas (filter #(is-type? % "Agenda") drawn)]
                       (continue-ability state side (pdhelper agendas 0) card nil)))}}})})
