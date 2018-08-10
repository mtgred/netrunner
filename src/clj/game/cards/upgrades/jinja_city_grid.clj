(in-ns 'game.cards.upgrades)

(def card-definition-jinja-city-grid
  {"Jinja City Grid"
   (letfn [(install-ice [ice ices grids server]
             (let [remaining (remove-once #(= (:cid %) (:cid ice)) ices)]
             {:async true
              :effect (req (if (= "None" server)
                             (continue-ability state side (choose-ice remaining grids) card nil)
                             (do (system-msg state side (str "reveals that they drew " (:title ice)))
                                 (wait-for (corp-install state side ice server {:extra-cost [:credit -4]})
                                           (if (= 1 (count ices))
                                             (effect-completed state side eid)
                                             (continue-ability state side (choose-ice remaining grids)
                                                               card nil))))))}))
           (choose-grid [ice ices grids]
             (if (= 1 (count grids))
               (install-ice ice ices grids (-> (first grids) :zone second zone->name))
               {:async true
                :prompt (str "Choose a server to install " (:title ice))
                :choices (conj (mapv #(-> % :zone second zone->name) grids) "None")
                :effect (effect (continue-ability (install-ice ice ices grids target) card nil))}))
           (choose-ice [ices grids]
             (if (empty? ices)
               nil
               {:async true
                :prompt "Choose an ice to reveal and install, or None to decline"
                :choices (conj (mapv :title ices) "None")
                :effect (req (if (= "None" target)
                               (effect-completed state side eid)
                               (continue-ability state side
                                                 (choose-grid (some #(when (= target (:title %)) %) ices)
                                                              ices grids)
                                                 card nil)))}))]
     {:events {:corp-draw {;; THIS IS A HACK: it prevents multiple Jinja from showing the "choose a server to install into" sequence
                           :once :per-turn
                           :once-key :jinja-city-grid-draw
                           :async true
                           :effect (req (cond
                                          ;; If ice were drawn, do the full routine.
                                          (some #(is-type? % "ICE") (:most-recent-drawn corp-reg))
                                          (let [ices (filter #(and (is-type? % "ICE")
                                                                   (get-card state %))
                                                             (:most-recent-drawn corp-reg))
                                                grids (filterv #(= "Jinja City Grid" (:title %))
                                                               (all-active-installed state :corp))]
                                            (when (= :runner (:active-player @state))
                                              (show-wait-prompt state :runner "Corp to resolve Jinja City Grid"))
                                            (if (not-empty ices)
                                              (continue-ability state side (choose-ice ices grids) card nil)
                                              (effect-completed state side eid)))
                                          ;; else, if it's the runner's turn, show a fake prompt so the runner can't infer that ice weren't drawn
                                          (= :runner (:active-player @state))
                                          (continue-ability
                                            state :corp
                                            {:prompt "You did not draw any ice to use with Jinja City Grid"
                                             :choices ["Carry on!"]
                                             :prompt-type :bogus
                                             :effect nil}
                                            card nil)
                                          ;; otherwise, we done
                                          :else
                                          (effect-completed state side eid)))}
               :post-corp-draw {:effect (req (swap! state dissoc-in [:per-turn :jinja-city-grid-draw])
                                             (when (= :runner (:active-player @state))
                                               (clear-wait-prompt state :runner)))}}})})
