(in-ns 'game.core)

(def card-definitions-operations-threat-level-alpha
  {"Threat Level Alpha"
   {:trace {:base 1
            :label "Give the Runner X tags"
            :delayed-completion true
            :effect (req (let [tags (-> @state :runner :tag)]
                           (if (pos? tags)
                             (do (tag-runner state :runner eid tags)
                                 (system-msg state side (str "uses Threat Level Alpha to give the Runner " tags " tags")))
                             (do (tag-runner state :runner eid 1)
                                 (system-msg state side "uses Threat Level Alpha to give the Runner a tag")))))}}})
