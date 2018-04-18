(in-ns 'game.core)

(def card-definitions-resources-dj-fenris
  {"DJ Fenris"
   (let [is-draft-id? #(.startsWith (:code %) "00")
         can-host? (fn [runner c] (and (is-type? c "Identity")
                                       (has-subtype? c "G-Mod")
                                       (not= (-> runner :identity :faction) (:faction c))
                                       (not (is-draft-id? c))))
         fenris-effect {:prompt "Choose a g-mod identity to host on DJ Fenris"
                        :choices (req (cancellable (filter (partial can-host? runner) (vals @all-cards)) :sorted))
                        :msg (msg "host " (:title target))
                        :effect (req (let [card (assoc-host-zones card)
                                           ;; Work around for get-card and update!
                                           c (assoc target :type "Fake-Identity")
                                           c (make-card c)
                                           c (assoc c :host (dissoc card :hosted)
                                                      :zone '(:onhost)
                                                      ;; semi hack to get deactivate to work
                                                      :installed true)]

                                       ;; Manually host id on card
                                       (update! state side (assoc card :hosted [c]))
                                       (card-init state :runner c)

                                       (clear-wait-prompt state :corp)
                                       (effect-completed state side eid)))}]
     {:delayed-completion true
      :effect (req (show-wait-prompt state :corp "Runner to pick identity to host on DJ Fenris")
                   (continue-ability state side fenris-effect card nil))})})
