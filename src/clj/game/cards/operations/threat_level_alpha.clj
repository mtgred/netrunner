(in-ns 'game.cards.operations)

(def card-definition-threat-level-alpha
  {"Threat Level Alpha"
   {:trace {:base 1
            :successful
            {:label "Give the Runner X tags"
             :async true
             :effect (req (let [tags (-> @state :runner :tag)]
                            (if (pos? tags)
                              (do (gain-tags state :corp eid tags)
                                  (system-msg
                                    state side
                                    (str "uses Threat Level Alpha to give the Runner " tags " tags")))
                              (do (gain-tags state :corp eid 1)
                                  (system-msg
                                    state side
                                    "uses Threat Level Alpha to give the Runner a tag")))))}}}})
