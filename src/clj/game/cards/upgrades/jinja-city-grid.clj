(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-jinja-city-grid
  {"Jinja City Grid"
   (letfn [(install-ice [ice ices grids server]
             (let [remaining (remove-once #(= (:cid %) (:cid ice)) ices)]
             {:delayed-completion true
              :effect (req (if (= "None" server)
                             (continue-ability state side (choose-ice remaining grids) card nil)
                             (do (system-msg state side (str "reveals that they drew " (:title ice)))
                                 (when-completed (corp-install state side ice server {:extra-cost [:credit -4]})
                                                 (if (= 1 (count ices))
                                                   (effect-completed state side eid)
                                                   (continue-ability state side (choose-ice remaining grids)
                                                                     card nil))))))}))

           (choose-grid [ice ices grids]
             (if (= 1 (count grids))
               (install-ice ice ices grids (-> (first grids) :zone second zone->name))
               {:delayed-completion true
                :prompt (str "Choose a server to install " (:title ice))
                :choices (conj (mapv #(-> % :zone second zone->name) grids) "None")
                :effect (effect (continue-ability (install-ice ice ices grids target) card nil))}))

           (choose-ice [ices grids]
             (if (empty? ices)
               nil
               {:delayed-completion true
                :prompt "Choose an ice to reveal and install, or None to decline"
                :choices (conj (mapv :title ices) "None")
                :effect (req (if (= "None" target)
                               (effect-completed state side eid)
                               (continue-ability state side
                                                 (choose-grid (some #(when (= target (:title %)) %) ices)
                                                              ices grids)
                                                 card nil)))}))]

     {:events {:corp-draw {:req (req (some #(is-type? % "ICE")
                                           (:most-recent-drawn corp-reg)))
                           ;; THIS IS A HACK: it prevents multiple Jinja from showing the "choose a server to install into" sequence
                           :once :per-turn
                           :once-key :jinja-city-grid-draw
                           :delayed-completion true
                           :effect (req (let [ices (filter #(and (is-type? % "ICE")
                                                                 (get-card state %))
                                                           (:most-recent-drawn corp-reg))
                                              grids (filterv #(= "Jinja City Grid" (:title %))
                                                             (all-active-installed state :corp))]
                                          (if (not-empty ices)
                                            (continue-ability state side (choose-ice ices grids) card nil)
                                            (effect-completed state side eid))))}
               :post-corp-draw {:effect (req (swap! state dissoc-in [:per-turn :jinja-city-grid-draw]))}}})})