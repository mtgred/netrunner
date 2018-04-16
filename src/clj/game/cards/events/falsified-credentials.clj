(in-ns 'game.core)

(declare run-event)

(def card-events-falsified-credentials
  {"Falsified Credentials"
   {:prompt "Choose a type"
    :choices ["Agenda" "Asset" "Upgrade"]
    :msg (msg "to guess " target)
    :delayed-completion true
    :effect (effect
             (continue-ability
              (let [chosen-type target]
                {:choices {:req #(let [topmost (get-nested-host %)]
                                   (and (is-remote? (second (:zone topmost)))
                                        (= (last (:zone topmost)) :content)
                                        (not (rezzed? %))))}
                 :delayed-completion true
                 :effect (req             ;taken from Drive By - maybe refactor
                          (when-completed (expose state side target)
                            (if (and async-result ;; expose was successful
                                     (= chosen-type (:type target)))
                              (continue-ability
                                  state :runner
                                  {:effect (effect (gain :credit 5))
                                   :msg "gain 5 [Credits] "}
                                  card nil)
                              (effect-completed state side eid))))})
              card nil))}})