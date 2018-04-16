(in-ns 'game.core)

(declare run-event)

(def card-events-dianas-hunt
  {"Dianas Hunt"
   {:implementation "One program per encounter not enforced"
    :prompt "Choose a server"
    :msg "make a run and install a program on encounter with each ICE"
    :choices (req runnable-servers)
    :delayed-completion true
    :abilities [{:label "Install a program using Diana's Hunt?"
                 :delayed-completion true
                 :effect (effect (resolve-ability
                                   {:prompt "Choose a program in your Grip to install"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "install " (:title target))
                                    :effect (req (let [diana-card (assoc-in target [:special :diana-installed] true)]
                                                   (runner-install state side diana-card {:no-cost true})
                                                   (swap! state update :diana #(conj % diana-card))))}
                                   card nil))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Diana's Hunt in the Temporary Zone to install a Program") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (req (let [hunt (:diana @state)]
                                                                                  (doseq [c hunt]
                                                                                    (let [installed (find-cid (:cid c) (all-installed state side))]
                                                                                      (when (get-in installed [:special :diana-installed])
                                                                                        (system-msg state side (str "trashes " (:title c) " at the end of the run from Diana's Hunt"))
                                                                                        (trash state side installed {:unpreventable true}))))
                                                                                  (swap! state dissoc :diana)
                                                                                  (unregister-events state side card)
                                                                                  (trash state side c)))}} c)))}
                      card nil))
    :events {:run-ends nil}}})
