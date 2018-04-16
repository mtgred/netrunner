(in-ns 'game.core)

(declare can-host?)

(def card-programs-wari
  {"Wari"
   (letfn [(prompt-for-subtype []
             {:prompt "Choose a subtype"
              :choices ["Barrier" "Code Gate" "Sentry"]
              :delayed-completion true
              :effect (req (when-completed (trash state side card {:unpreventable true})
                             (continue-ability state side
                                               (expose-and-maybe-bounce target)
                                               card nil)))})
           (expose-and-maybe-bounce [chosen-subtype]
             {:choices {:req #(and (ice? %) (not (rezzed? %)))}
              :delayed-completion true
              :msg (str "name " chosen-subtype)
              :effect (req (when-completed (expose state side target)
                             (do (if (and async-result
                                          (has-subtype? target chosen-subtype))
                                   (do (move state :corp target :hand)
                                       (system-msg state :runner
                                                   (str "add " (:title target) " to HQ"))))
                                 (effect-completed state side eid))))})]
     {:events {:successful-run
              {:interactive (req true)
               :delayed-completion true
               :req (req (and (= target :hq)
                              (first-successful-run-on-server? state :hq)
                              (some #(and (ice? %) (not (rezzed? %)))
                                    (all-installed state :corp))))
               :effect (effect (continue-ability
                                {:prompt "Use Wari?"
                                 :choices ["Yes" "No"]
                                 :delayed-completion true
                                 :effect (req (if (= target "Yes")
                                                (continue-ability state side
                                                                  (prompt-for-subtype)
                                                                  card nil)
                                                (effect-completed state side eid)))}
                                card nil))}}})})
