(in-ns 'game.cards.programs)

(def card-definition-wari
  {"Wari"
   (letfn [(prompt-for-subtype []
             {:prompt "Choose a subtype"
              :choices ["Barrier" "Code Gate" "Sentry"]
              :async true
              :effect (req (wait-for (trash state side card {:unpreventable true})
                                     (continue-ability state side
                                                       (expose-and-maybe-bounce target)
                                                       card nil)))})
           (expose-and-maybe-bounce [chosen-subtype]
             {:choices {:req #(and (ice? %) (not (rezzed? %)))}
              :async true
              :msg (str "name " chosen-subtype)
              :effect (req (wait-for (expose state side target)
                                     (do (if (and async-result
                                                  (has-subtype? target chosen-subtype))
                                           (do (move state :corp target :hand)
                                               (system-msg state :runner
                                                           (str "add " (:title target) " to HQ"))))
                                         (effect-completed state side eid))))})]
     {:events {:successful-run
              {:interactive (req true)
               :async true
               :req (req (and (= target :hq)
                              (first-successful-run-on-server? state :hq)
                              (some #(and (ice? %) (not (rezzed? %)))
                                    (all-installed state :corp))))
               :effect (effect (continue-ability
                                {:prompt "Use Wari?"
                                 :choices ["Yes" "No"]
                                 :async true
                                 :effect (req (if (= target "Yes")
                                                (continue-ability state side
                                                                  (prompt-for-subtype)
                                                                  card nil)
                                                (effect-completed state side eid)))}
                                card nil))}}})})
