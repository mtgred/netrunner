(ns game.core.sabotage
  (:require
    [clojure.string :as string]
    [game.core.card :refer [corp? in-hand?]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.moving :refer [trash-cards]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [req msg wait-for continue-ability]]
    [game.utils :refer [pluralize]]))

(defn sabotage-ability
  [n]
  (let [choosing-prompt (req
                          (let [cards-rd (count (get-in @state [:corp :deck]))
                                forced-hq (- n cards-rd)]
                            (str "Choose"
                                 (when (pos? forced-hq)
                                   (str " at least " forced-hq " " (pluralize "card" forced-hq) " and"))
                                 " up to " n " " (pluralize "card" n)
                                 " to trash from HQ. Remainder will be trashed from top of R&D.")))
        trash-req (req
                    (let [selected-hq (count targets)
                          selected-rd (min (count (:deck corp))
                                           (- n selected-hq))
                          ; to-trash (concat targets (take selected-rd (:deck corp)))]
                          to-trash targets]
                      (system-msg state side
                                  (str
                                    "trashes"
                                    (when selected-hq
                                      (str " " selected-hq " " (pluralize "card" selected-hq) " from HQ"))
                                    (when (and selected-hq selected-rd)
                                      " and")
                                    (when selected-rd
                                      (str " " selected-rd " " (pluralize "card" selected-rd) " from the top of R&D"))))
                      (wait-for (trash-cards state side to-trash {:unpreventable true})
                                (effect-completed state side eid))))
        choosing-ab (fn [forced-hq]
                      {:waiting-prompt "Corp to choose an option"
                       :player :corp
                       :prompt choosing-prompt
                       :choices {;:min forced-hq
                                 :max n
                                 :card #(and (corp? %)
                                             (in-hand? %))}
                       :async true
                       :effect trash-req})
        check-forcing-ab {:async true
                          :effect (req
                                    (let [cards-rd (count (:deck corp))
                                          cards-hq (count (:hand corp))
                                          forced-hq (- n cards-rd)]
                                      (if (> forced-hq cards-hq)
                                        (do (trash-req state :corp eid card (:hand corp))
                                            (effect-completed state side eid))
                                        (resolve-ability state side eid
                                                         (choosing-ab forced-hq)
                                                         card nil))))}]
    {:req (req (> n 0))
     :msg (msg "sabotage " n)
     :async true
     :effect (req (wait-for
                    (resolve-ability state side check-forcing-ab card targets)
                    (effect-completed state side eid)))}))
