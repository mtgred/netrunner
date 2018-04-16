(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-film-critic
  {"Film Critic"
   (letfn [(get-agenda [card] (first (filter #(= "Agenda" (:type %)) (:hosted card))))]
     {:implementation "Use hosting ability when presented with Access prompt for an agenda"
      :abilities [{:req (req (and (empty? (filter #(= "Agenda" (:type %)) (:hosted card)))
                                  (is-type? (:card (first (get-in @state [side :prompt]))) "Agenda")))
                   :label "Host an agenda being accessed"
                   :effect (req (when-let [agenda (:card (first (get-in @state [side :prompt])))]
                                  (host state side card (move state side agenda :play-area))
                                  (trigger-event state side :no-steal agenda)
                                  (close-access-prompt state side)
                                  (effect-completed state side eid nil)
                                  (when-not (:run @state)
                                    (swap! state dissoc :access))))
                   :msg (msg "host " (:title (:card (first (get-in @state [side :prompt])))) " instead of accessing it")}
                  {:cost [:click 2] :label "Add hosted agenda to your score area"
                   :req (req (not (empty? (:hosted card))))
                   :effect (req (let [c (move state :runner (get-agenda card) :scored)]
                                  (gain-agenda-point state :runner (get-agenda-points state :runner c))))
                   :msg (msg (let [c (get-agenda card)]
                               (str "add " (:title c) " to their score area and gain "
                                    (quantify (get-agenda-points state :runner c) "agenda point"))))}]})})