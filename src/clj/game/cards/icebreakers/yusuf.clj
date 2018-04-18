(in-ns 'game.core)

(def card-definitions-icebreakers-yusuf
  {"Yusuf"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (system-msg "adds 1 virus counter to Yusuf")
                                              (add-counter card :virus 1))}}
    :abilities [{:label "Add strength"
                 :prompt "Choose a card with virus counters"
                 :choices {:req #(pos? (get-in % [:counter :virus] 0))}
                 :effect (req (let [selected-virus target
                                    yusuf card
                                    counters (get-in selected-virus [:counter :virus] 0)]
                                (resolve-ability state side
                                                 {:prompt "Spend how many counters?"
                                                  :choices {:number (req counters)
                                                            :default (req 1)}
                                                  :effect (req (let [cost target]
                                                                 (resolve-ability
                                                                   state side
                                                                   {:counter-cost [:virus cost]
                                                                    :effect (effect (pump (get-card state yusuf) cost)
                                                                                    (system-msg (str "spends " cost (pluralize " counter" cost) " from " (:title selected-virus)
                                                                                                     " to add " cost " strength to Yusuf")))}
                                                                   selected-virus nil)))}
                                                 yusuf nil)))}
                {:label "Break barrier subroutine(s)"
                 :prompt "Choose a card with virus counters"
                 :choices {:req #(pos? (get-in % [:counter :virus] 0))}
                 :effect (req (let [selected-virus target
                                    yusuf card
                                    counters (get-in selected-virus [:counter :virus] 0)]
                                (resolve-ability state side
                                                 {:prompt "Spend how many counters?"
                                                  :choices {:number (req counters)
                                                            :default (req 1)}
                                                  :effect (req (let [cost target]
                                                                 (resolve-ability
                                                                   state side
                                                                   {:counter-cost [:virus cost]
                                                                    :effect (effect (system-msg (str "spends " cost (pluralize " counter" cost) " from " (:title selected-virus)
                                                                                                     " to break " cost (pluralize " barrier subroutine" cost) " with Yusuf")))}
                                                                   selected-virus nil)))}
                                                 yusuf nil)))}]}})
