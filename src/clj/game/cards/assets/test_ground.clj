(in-ns 'game.cards.assets)

(def card-definition-test-ground
  {"Test Ground"
   (letfn [(derez-card [advancements]
             {:async true
              :prompt "Derez a card"
              :choices {:req #(and (installed? %)
                                   (rezzed? %))}
              :effect (req (derez state side target)
                           (if (pos? (dec advancements))
                             (continue-ability state side (derez-card (dec advancements)) card nil)
                             (effect-completed state side eid)))})]
     {:advanceable :always
      :abilities [{:label "Derez 1 card for each advancement token"
                   :req (req (pos? (get-counters card :advancement)))
                   :msg (msg "derez " (quantify (get-counters card :advancement) "card"))
                   :effect (req (let [advancements (get-counters card :advancement)]
                                  (trash state side card {:cause :ability-cost})
                                  (show-wait-prompt state :runner (str "Corp to derez "
                                                                       (quantify advancements "card")))
                                  (wait-for (resolve-ability state side (derez-card advancements) card nil)
                                            (clear-wait-prompt state :runner))))}]})})
