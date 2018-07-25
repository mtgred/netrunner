(in-ns 'game.cards.programs)

(def card-definition-surfer
  {"Surfer"
   (letfn [(surf [state cice]
             {:prompt (msg "Choose an ICE before or after " (:title cice))
              :choices {:req #(and (ice? %)
                                   (= (:zone %) (:zone cice))
                                   (= 1 (abs (- (ice-index state %)
                                                (ice-index state cice)))))}
              :msg "swap a piece of Barrier ICE"
              :effect (req (let [tgtndx (ice-index state target)
                                 cidx (ice-index state cice)]
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % tgtndx cice))
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % cidx target))
                             (swap! state update-in [:run] #(assoc % :position (inc tgtndx)))
                             (update-all-ice state side)
                             (trigger-event state side :approach-ice current-ice)))})]
     {:abilities [{:cost [:credit 2]
                   :req (req (and (:run @state)
                                  (rezzed? current-ice)
                                  (has-subtype? current-ice "Barrier")))
                   :label "Swap the Barrier ICE currently being encountered with a piece of ICE directly before or after it"
                   :effect (effect (resolve-ability (surf state current-ice) card nil))}]})})
