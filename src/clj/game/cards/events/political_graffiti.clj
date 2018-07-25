(in-ns 'game.cards.events)

(def card-definition-political-graffiti
  {"Political Graffiti"
   (let [update-agenda-points (fn [state side target amount]
                               (set-prop state side (get-card state target) :agendapoints (+ amount (:agendapoints (get-card state target))))
                               (gain-agenda-point state side amount))]
     {:req (req archives-runnable)
      :events {:purge {:effect (effect (trash card {:cause :purge}))}}
      :trash-effect {:effect (req (let [current-side (get-scoring-owner state {:cid (:agenda-cid card)})]
                                    (update-agenda-points state current-side (find-cid (:agenda-cid card) (get-in @state [current-side :scored])) 1)))}
      :effect (effect (run :archives
                        {:req (req (= target :archives))
                         :replace-access
                         {:prompt "Select an agenda to host Political Graffiti"
                          :choices {:req #(in-corp-scored? state side %)}
                          :msg (msg "host Political Graffiti on " (:title target) " as a hosted condition counter")
                          :effect (req (host state :runner (get-card state target)
                                         ; keep host cid in :agenda-cid because `trash` will clear :host
                                         (assoc card :zone [:discard] :installed true :agenda-cid (:cid (get-card state target))))
                                       (update-agenda-points state :corp target -1))}} card))})})
