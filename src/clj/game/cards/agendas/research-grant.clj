(in-ns 'game.core)

(def card-definitions-agendas-research-grant
  {"Research Grant"
   {:interactive (req true)
    :silent (req (empty? (filter #(= (:title %) "Research Grant") (all-installed state :corp))))
    :req (req (not (empty? (filter #(= (:title %) "Research Grant") (all-installed state :corp)))))
    :delayed-completion true
    :effect (effect (continue-ability
                      {:prompt "Select another installed copy of Research Grant to score"
                       :choices {:req #(= (:title %) "Research Grant")}
                       :delayed-completion true
                       :effect (effect (set-prop target :advance-counter (:advancementcost target))
                                       (score eid (get-card state target)))
                       :msg "score another installed copy of Research Grant"}
                     card nil))}})
