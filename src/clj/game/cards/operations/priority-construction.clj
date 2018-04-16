(in-ns 'game.core)

(def card-operations-priority-construction
  {"Priority Construction"
   (letfn [(install-card [chosen]
            {:prompt "Select a remote server"
             :choices (req (conj (vec (get-remote-names @state)) "New remote"))
             :delayed-completion true
             :effect (effect (corp-install (assoc chosen :advance-counter 3) target {:no-install-cost true}))})]
     {:delayed-completion true
      :prompt "Choose a piece of ICE in HQ to install"
      :choices {:req #(and (in-hand? %) (= (:side %) "Corp") (ice? %))}
      :msg "install an ICE from HQ and place 3 advancements on it"
      :cancel-effect (req (effect-completed state side eid))
      :effect (effect (continue-ability (install-card target) card nil))})})