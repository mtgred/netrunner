(in-ns 'game.core)

(declare run-event)

(def card-events-hostage
  {"Hostage"
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their Grip and shuffle their Stack")
    :effect (req (let [connection target]
                   (trigger-event state side :searched-stack nil)
                   (resolve-ability
                     state side
                     {:prompt (str "Install " (:title connection) "?")
                      :choices ["Yes" "No"]
                      :effect (req (let [d target]
                                     (resolve-ability state side
                                       {:effect (req (shuffle! state side :deck)
                                                     (if (= "Yes" d)
                                                       (runner-install state side connection)
                                                       (move state side connection :hand)))} card nil)))}
                     card nil)))}})