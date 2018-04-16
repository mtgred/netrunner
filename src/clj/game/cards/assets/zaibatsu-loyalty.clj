(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-zaibatsu-loyalty
  {"Zaibatsu Loyalty"
   {:prevent {:expose [:all]}
    :derezzed-events
    {:pre-expose
     {:delayed-completion true
      :effect (req (let [etarget target]
                     (continue-ability state side
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
