(in-ns 'game.cards.resources)

(def card-definition-no-one-home
  {"No One Home"
   (letfn [(first-chance? [state side]
             (< (+ (event-count state side :pre-tag)
                   (event-count state side :pre-damage))
                2))
           (start-trace [type]
             (let [message (str "avoid any " (if (= type :net)
                                               "amount of net damage"
                                               "number of tags"))]
               {:player :corp
                :label (str "Trace 0 - if unsuccessful, " message)
                :trace {:base 0
                        :priority 11
                        :unsuccessful {:msg message
                                       :effect (req (if (= type :net)
                                                      (damage-prevent state side :net Integer/MAX_VALUE)
                                                      (tag-prevent state :runner Integer/MAX_VALUE)))}}}))]
     {:interactions {:prevent [{:type #{:net :tag}
                                :req (req (first-chance? state side))}]}
      :abilities [{:msg "force the Corp to trace"
                   :async true
                   :effect (req (let [type (get-in @state [:prevent :current])]
                                  (wait-for (trash state side card {:unpreventable true})
                                            (continue-ability state side (start-trace type)
                                                              card nil))))}]})})
