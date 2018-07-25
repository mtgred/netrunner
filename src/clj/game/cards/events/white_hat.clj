(in-ns 'game.cards.events)

(def card-definition-white-hat
  {"White Hat"
   (letfn [(finish-choice [choices]
             (let [choices (filter #(not= "None" %) choices)]
               (when (not-empty choices)
                {:effect (req (doseq [c choices]
                                (move state :corp c :deck))
                              (shuffle! state :corp :deck))
                 :msg (str "shuffle " (join ", " (map :title choices)) " into R&D")})))
           (choose-cards [hand chosen]
             {:prompt "Choose a card in HQ to shuffle into R&D"
              :player :runner
              :choices (conj (vec (clojure.set/difference hand chosen))
                             "None")
              :async true
              :effect (req (if (and (empty? chosen)
                                    (not= "None" target))
                             (continue-ability state side (choose-cards hand (conj chosen target)) card nil)
                             (continue-ability state side (finish-choice (conj chosen target)) card nil)))})]
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :trace {:base 3
            :unsuccessful
            {:async true
             :msg "reveal all cards in HQ"
             :effect (effect (continue-ability :runner (choose-cards (set (:hand corp)) #{}) card nil))}}})})
