(in-ns 'game.cards.identities)

(def card-definition-acme-consulting-the-truth-you-need
  {"Acme Consulting: The Truth You Need"
   (letfn [(activate [state card active]
             (update! state :corp (assoc-in card [:special :acme-active] active))
             (swap! state update-in [:runner :additional-tag] (if active inc dec)))
           (outermost? [run-position run-ices]
             (and run-position
                  (pos? run-position)
                  (= run-position (count run-ices))))]
     {:implementation "Tag is gained on approach, not on encounter"
      :events {:run {:effect (req (when (and (outermost? run-position run-ices)
                                             (rezzed? current-ice))
                                    (activate state card true)))}
               :rez {:effect (req (when (outermost? run-position run-ices)
                                    (activate state card true)))}
               :derez {:effect (req (when (outermost? run-position run-ices)
                                      (activate state card false)))}
               :pass-ice {:effect (req (when (and (outermost? run-position run-ices)
                                                  (get-in card [:special :acme-active]))
                                         (activate state card false)))}
               :end-run {:effect (req (when (get-in card [:special :acme-active])
                                        (activate state card false)))}}})})
