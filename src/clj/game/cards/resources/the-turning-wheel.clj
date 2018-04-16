(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-the-turning-wheel
  {"The Turning Wheel"
   {:events {:agenda-stolen {:effect (effect (update! (assoc card :agenda-stolen true)))
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
                 :effect  (req (swap! state update-in [:run :ttw-spent] (fnil inc 0))
                               (register-events state side
                                                {:pre-access {:req (req (and (get-in @state [:run :ttw-spent]) (#{:hq :rd} target)))
                                                              :effect (effect (access-bonus 1)
                                                                              (unregister-events #(find-latest state card) {:events {:pre-access nil}}))
                                                              :silent (req true)}} #(find-latest state card)))}]}})
