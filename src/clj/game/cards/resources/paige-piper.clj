(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-paige-piper
  {"Paige Piper"
   (let [pphelper (fn [title cards]
                    (let [num (count cards)]
                      {:optional
                       {:prompt (str "Use Paige Piper to trash copies of " title "?")
                        :yes-ability {:prompt "How many would you like to trash?"
                                      :choices (take (inc num) ["0" "1" "2" "3" "4" "5"])
                                      :msg "shuffle their Stack"
                                      :effect (req (let [target (str->int target)]
                                                     (trigger-event state side :searched-stack nil)
                                                     (shuffle! state :runner :deck)
                                                     (doseq [c (take target cards)]
                                                       (trash state side c {:unpreventable true}))
                                                     (when (> (int target) 0)
                                                       (system-msg state side (str "trashes "
                                                                                   (quantify target "cop" "y" "ies")
                                                                                   " of " title)))))}}}))]
     {:events {:runner-install {:req (req (first-event? state side :runner-install))
                                :delayed-completion true
                                :effect (effect (continue-ability
                                                 (pphelper (:title target)
                                                           (->> (:deck runner)
                                                                (filter #(has? % :title (:title target)))
                                                                (vec)))
                                                 card nil))}}})
   "Patron"
   (let [ability {:prompt "Choose a server for Patron" :choices (req (conj servers "No server"))
                  :req (req (and (not (click-spent? :runner state)) (not (used-this-turn? (:cid card) state))))
                  :msg (msg "target " target)
                  :effect (req (when (not= target "No server")
                                 (update! state side (assoc card :server-target target))))}]
     {:events {:runner-turn-begins ability
               :successful-run
               {:req (req (= (zone->name (get-in @state [:run :server])) (:server-target (get-card state card))))
                :once :per-turn
                :effect (req (let [st card]
                               (swap! state assoc-in [:run :run-effect :replace-access]
                                      {:mandatory true
                                       :effect (effect (resolve-ability
                                                         {:msg "draw 2 cards instead of accessing"
                                                          :effect (effect (draw 2)
                                                                          (update! (dissoc st :server-target)))}
                                                         st nil))})))}
               :runner-turn-ends {:effect (effect (update! (dissoc card :server-target)))}}
      :abilities [ability]})})
