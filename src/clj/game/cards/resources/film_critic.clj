(in-ns 'game.cards.resources)

(def card-definition-film-critic
  {"Film Critic"
   (letfn [(get-agenda [card] (first (filter #(= "Agenda" (:type %)) (:hosted card))))
           (host-agenda? [agenda]
             {:optional {:prompt (str "You access " (:title agenda) ". Host it on Film Critic?")
                        :yes-ability {:effect (req (host state side card (move state side agenda :play-area))
                                                   (access-end state side eid agenda)
                                                   (when-not (:run @state)
                                                     (swap! state dissoc :access)))
                                      :msg (msg "host " (:title agenda) " instead of accessing it")}}})]
     {:events {:access {:req (req (and (empty? (filter #(= "Agenda" (:type %)) (:hosted card)))
                                       (is-type? target "Agenda")))
                        :interactive (req true)
                        :async true
                        :effect (effect (continue-ability (host-agenda? target) card nil))}}
      :abilities [{:cost [:click 2] :label "Add hosted agenda to your score area"
                   :req (req (get-agenda card))
                   :async true
                   :effect (req (let [c (get-agenda card)
                                      points (get-agenda-points state :runner c)]
                                  (as-agenda state :runner eid c points)))
                   :msg (msg (let [c (get-agenda card)]
                               (str "add " (:title c) " to their score area and gain "
                                    (quantify (get-agenda-points state :runner c) "agenda point"))))}]})})
