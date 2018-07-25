(in-ns 'game.cards.assets)

(def card-definition-daily-business-show
  {"Daily Business Show"
   {:events {:pre-corp-draw
             {:msg "draw additional cards"
              ;; The req catches draw events that happened before DBS was rezzed.
              :req (req (first-event? state :corp :pre-corp-draw))
              ;; The once and once-key force a single DBS to act on behalf of all rezzed DBS's.
              :once :per-turn
              :once-key :daily-business-show-draw-bonus
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %))
                                                          (rezzed? %))
                                                    (all-installed state :corp)))]
                             (draw-bonus state side dbs)))}
             :post-corp-draw
             {:req (req (first-event? state :corp :post-corp-draw))
              :once :per-turn
              :once-key :daily-business-show-put-bottom
              :async true
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %))
                                                          (rezzed? %))
                                                    (all-installed state :corp)))
                                 drawn (get-in @state [:corp :register :most-recent-drawn])]
                             (show-wait-prompt state :runner "Corp to use Daily Business Show")
                             (wait-for (resolve-ability
                                         state side
                                         {:prompt (str "Select " (quantify dbs "card") " to add to the bottom of R&D")
                                          :msg (msg "add " (quantify dbs "card") " to the bottom of R&D")
                                          :choices {:max dbs
                                                    :req #(some (fn [c] (= (:cid c) (:cid %))) drawn)}
                                          :effect (req (doseq [c targets]
                                                         (move state side c :deck)))}
                                         card targets)
                                       (do (clear-wait-prompt state :runner)
                                           (effect-completed state side eid)))))}}}})
