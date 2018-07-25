(in-ns 'game.cards.upgrades)

(def card-definition-arella-salvatore
  {"Arella Salvatore"
   (let [select-ability
         {:prompt "Select a card to install with Arella Salvatore"
          :choices {:req #(and (corp-installable-type? %)
                               (in-hand? %)
                               (= (:side %) "Corp"))}
          :async true
          :cancel-effect (req (effect-completed state side eid))
          :effect (req (wait-for (corp-install state :corp target nil {:no-install-cost true :display-message false})
                                 (let [inst-target (find-latest state target)]
                                   (add-prop state :corp inst-target :advance-counter 1 {:placed true})
                                   (system-msg state :corp
                                               (str "uses Arella Salvatore to install and place a counter on "
                                                    (card-str state inst-target) ", ignoring all costs"))
                                   (effect-completed state side eid))))}]
     {:events
      {:agenda-scored
       {:req (req (and (= (:previous-zone target) (:zone card))))
        :interactive (req true)
        :silent (req (empty? (filter corp-installable-type? (:hand corp))))
        :async true
        :effect (req (if (some corp-installable-type? (:hand corp))
                       (continue-ability state side select-ability card nil)
                       (effect-completed state side eid)))}}})})
