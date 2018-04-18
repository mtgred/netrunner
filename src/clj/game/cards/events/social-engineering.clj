(in-ns 'game.core)

(def card-definitions-events-social-engineering
  {"Social Engineering"
   {:prompt "Select an unrezzed piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (not (rezzed? %)) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))]
              (resolve-ability
                 state :runner
                 {:msg (msg "select the piece of ICE at position " (ice-index state ice) " of " serv)
                  :effect (effect (register-events {:pre-rez-cost
                                                    {:req (req (= target ice))
                                                     :effect (req (let [cost (rez-cost state side (get-card state target))]
                                                                    (gain state :runner :credit cost)))
                                                     :msg (msg "gain " (rez-cost state side (get-card state target)) " [Credits]")}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:pre-rez-cost nil}
    :end-turn {:effect (effect (unregister-events card))}}})
