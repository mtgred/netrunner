(in-ns 'game.cards.resources)

(def card-definition-the-turning-wheel
  {"The Turning Wheel"
   {:events {:agenda-stolen {:effect (effect (update! (assoc card :agenda-stolen true)))
                             :silent (req true)}
             :pre-access {:req (req (and (:run @state)
                                         (pos? (get-in @state [:run :ttw-bonus] 0))))
                          :effect (req (let [ttw-bonus (get-in @state [:run :ttw-bonus] 0)
                                             deferred-bonus (get-in @state [:run :ttw-deferred-bonus] 0)]
                                         (if (#{:hq :rd} target)
                                           (when (pos? deferred-bonus)
                                             (access-bonus state side ttw-bonus)
                                             (swap! state assoc-in [:run :ttw-deferred-bonus] 0))
                                           (when (zero? deferred-bonus)
                                             (access-bonus state side (- ttw-bonus))
                                             (swap! state assoc-in [:run :ttw-deferred-bonus] ttw-bonus)))))
                          :silent (req true)}
             :run-ends {:effect (req (when (and (not (:agenda-stolen card))
                                                (#{:hq :rd} target))
                                       (add-counter state side card :power 1)
                                       (system-msg state :runner (str "places a power counter on " (:title card))))
                                     (update! state side (dissoc (get-card state card) :agenda-stolen)))
                        :silent (req true)}}
    :abilities [{:counter-cost [:power 2]
                 :req (req (:run @state))
                 :msg "access 1 additional card from HQ or R&D for the remainder of the run"
                 :effect  (req (swap! state update-in [:run :ttw-bonus] (fnil inc 0))
                               (access-bonus state side 1))}]}})
