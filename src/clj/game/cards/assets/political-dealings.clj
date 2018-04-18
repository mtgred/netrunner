(in-ns 'game.core)

(def card-definitions-assets-political-dealings
  {"Political Dealings"
   (let [pdhelper (fn pd [agendas n]
                    {:optional
                     {:prompt (msg "Reveal and install " (:title (nth agendas n)) "?")
                      :yes-ability {:delayed-completion true
                                    :msg (msg "reveal " (:title (nth agendas n)))
                                    :effect (req (when-completed
                                                   (corp-install state side (nth agendas n) nil
                                                                 {:install-state
                                                                  (:install-state (card-def (nth agendas n))
                                                                    :unrezzed)})
                                                   (if (< (inc n) (count agendas))
                                                     (continue-ability state side (pd agendas (inc n)) card nil)
                                                     (effect-completed state side eid))))}
                      :no-ability {:delayed-completion true
                                   :effect (req (if (< (inc n) (count agendas))
                                                  (continue-ability state side (pd agendas (inc n)) card nil)
                                                  (effect-completed state side eid)))}}})]
     {:events
      {:corp-draw
       {:delayed-completion true
        :req (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                        agendas (filter #(is-type? % "Agenda") drawn)]
                    (seq agendas)))
        :effect (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                           agendas (filter #(is-type? % "Agenda") drawn)]
                       (continue-ability state side (pdhelper agendas 0) card nil)))}}})})
