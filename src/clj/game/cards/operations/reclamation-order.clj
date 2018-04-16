(in-ns 'game.core)

(def card-operations-reclamation-order
  {"Reclamation Order"
   {:prompt "Select a card from Archives"
    :show-discard true
    :choices {:req #(and (= (:side %) "Corp")
                         (not= (:title %) "Reclamation Order")
                         (= (:zone %) [:discard]))}
    :msg (msg "name " (:title target))
    :effect (req (let [title (:title target)
                       cards (filter #(= title (:title %)) (:discard corp))
                       n (count cards)]
                   (continue-ability state side
                                     {:prompt (str "Choose how many copies of "
                                                   title " to reveal")
                                      :choices {:number (req n)}
                                      :msg (msg "reveal "
                                                (quantify target "cop" "y" "ies")
                                                " of " title
                                                " from Archives"
                                                (when (pos? target)
                                                  (str " and add "
                                                       (if (= 1 target) "it" "them")
                                                       " to HQ")))
                                      :effect (req (doseq [c (->> cards
                                                                  (sort-by :seen)
                                                                  reverse
                                                                  (take target))]
                                                     (move state side c :hand)))}
                                     card nil)))}})