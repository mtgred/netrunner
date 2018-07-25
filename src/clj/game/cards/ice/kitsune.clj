(in-ns 'game.cards.ice)

(def card-definition-kitsune
  {"Kitsune"
   {:subroutines [{:prompt "Select a card in HQ to force access"
                   :choices {:req in-hand?}
                   :label "Force the Runner to access a card in HQ"
                   :msg (msg "force the Runner to access " (:title target))
                   :effect (req (trash state side card)
                                (wait-for (trigger-event-sync state side :pre-access :hq)
                                          (wait-for (access-card state side target)
                                                    (let [from-hq (dec (access-count state side :hq-access))]
                                                      (continue-ability
                                                        state :runner
                                                        (access-helper-hq
                                                          state from-hq
                                                          ;; access-helper-hq uses a set to keep track of which cards have already
                                                          ;; been accessed. by adding HQ root's contents to this set, we make the runner
                                                          ;; unable to access those cards, as Kitsune intends.
                                                          (conj (set (get-in @state [:corp :servers :hq :content])) target))
                                                        card nil)))))}]}})
