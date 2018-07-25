(in-ns 'game.cards.ice)

(def card-definition-kamali-1-0
  {"Kamali 1.0"
   (letfn [(better-name [kind] (if (= "hardware" kind) "piece of hardware" kind))
           (runner-trash [kind]
             {:prompt (str "Select an installed " (better-name kind) " to trash")
              :label (str "Trash an installed " (better-name kind))
              :msg (msg "trash " (:title target))
              :async true
              :choices {:req #(and (installed? %)
                                   (is-type? % (capitalize kind)))}
              :cancel-effect (effect (system-msg (str "fails to trash an installed " (better-name kind)))
                                     (effect-completed eid))
              :effect (effect (trash eid target {:cause :subroutine}))})
           (sub-map [kind]
             {:player :runner
              :async true
              :prompt "Choose one"
              :choices ["Take 1 brain damage" (str "Trash an installed " (better-name kind))]
              :effect (req (if (= target "Take 1 brain damage")
                             (do (system-msg state :corp "uses Kamali 1.0 to give the Runner 1 brain damage")
                                 (damage state :runner eid :brain 1 {:card card}))
                             (continue-ability state :runner (runner-trash kind) card nil)))})
           (brain-trash [kind]
             {:label (str "Force the Runner to take 1 brain damage or trash an installed " (better-name kind))
              :msg (str "force the Runner to take 1 brain damage or trash an installed " (better-name kind))
              :async true
              :effect (req (show-wait-prompt state :corp "Runner to decide on Kamali 1.0 action")
                           (wait-for (resolve-ability state side (sub-map kind) card nil)
                                     (clear-wait-prompt state :corp)))})]
     {:subroutines [(brain-trash "resource")
                    (brain-trash "hardware")
                    (brain-trash "program")]
      :runner-abilities [(runner-break [:click 1] 1)]})})
