(in-ns 'game.core)

(def card-definitions-assets-malia-z0l0k4
  {"Malia Z0L0K4"
   (let [re-enable-target (req (when-let [malia-target (:malia-target card)]
                                 (system-msg state side (str "uses "  (:title card) " to unblank "
                                                             (card-str state malia-target)))
                                 (enable-card state :runner (get-card state malia-target))
                                 (when-let [reactivate-effect (:reactivate (card-def malia-target))]
                                   (resolve-ability state :runner reactivate-effect (get-card state malia-target) nil))))]
     {:effect (effect (update! (assoc card :malia-target target))
                      (disable-card :runner target))
      :msg (msg (str "blank the text box of " (card-str state target)))

      :choices {:req #(and (= (:side %) "Runner") (installed? %) (resource? %)
                           (not (has-subtype? % "Virtual")))}
      :leave-play re-enable-target
      :move-zone re-enable-target})})
