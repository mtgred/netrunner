(in-ns 'game.core)

(def card-definitions-operations-friends-in-high-places
  {"Friends in High Places"
   (let [fhelper (fn fhp [n] {:prompt "Select a card in Archives to install with Friends in High Places"
                              :priority -1
                              :delayed-completion true
                              :show-discard true
                              :choices {:req #(and (= (:side %) "Corp")
                                                   (not (is-type? % "Operation"))
                                                   (in-discard? %))}
                              :effect (req (when-completed
                                             (corp-install state side target nil nil)
                                             (do (system-msg state side (str "uses Friends in High Places to "
                                                                             (corp-install-msg target)))
                                                 (if (< n 2)
                                                   (continue-ability state side (fhp (inc n)) card nil)
                                                   (effect-completed state side eid card)))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (fhelper 1) card nil))})})
