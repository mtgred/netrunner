(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-illicit-sales
  {"Illicit Sales"
   {:delayed-completion true
    :effect (req (when-completed
                   (resolve-ability state side
                     {:optional
                      {:prompt "Take 1 bad publicity from Illicit Sales?"
                       :yes-ability {:msg "take 1 bad publicity"
                                     :effect (effect (gain-bad-publicity :corp 1))}
                       :no-ability {:effect (req (effect-completed state side eid))}}}
                     card nil)
                   (do (let [n (* 3 (+ (get-in @state [:corp :bad-publicity]) (:has-bad-pub corp)))]
                         (gain state side :credit n)
                         (system-msg state side (str "gains " n " [Credits] from Illicit Sales"))
                         (effect-completed state side eid)))))}})