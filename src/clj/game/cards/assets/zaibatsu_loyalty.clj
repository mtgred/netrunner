(in-ns 'game.cards.assets)

(def card-definition-zaibatsu-loyalty
  {"Zaibatsu Loyalty"
   {:interactions {:prevent [{:type #{:expose}
                              :req (req true)}]}
    :derezzed-events
    {:pre-expose
     {:async true
      :effect (req (let [etarget target]
                     (continue-ability
                       state side
                       {:optional {:req (req (not (rezzed? card)))
                                   :player :corp
                                   :prompt (msg "The Runner is about to expose " (:title etarget) ". Rez Zaibatsu Loyalty?")
                                   :yes-ability {:effect (effect (rez card))}}}
                       card nil)))}}
    :abilities [{:msg "prevent 1 card from being exposed"
                 :cost [:credit 1]
                 :effect (effect (expose-prevent 1))}
                {:msg "prevent 1 card from being exposed"
                 :label "[Trash]: Prevent 1 card from being exposed"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (expose-prevent 1))}]}})
