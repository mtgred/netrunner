(ns game.core.sabotage
  (:require
    [clojure.string :as string]
    [game.core.card :refer [corp? in-hand?]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.moving :refer [trash-cards]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [req msg continue-ability]]
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
                    (let [targets (if (nil? target) [] targets) ; catch cancel-effect that gives [nil] as targets
                          selected-hq (count targets)
                          selected-rd (min (count (:deck corp))
                                           (- n selected-hq))
                          to-trash (concat targets (take selected-rd (:deck corp)))]
                      (system-msg state side
                                  (str
                                    "trashes"
                                    (when selected-hq
                                      (str " " selected-hq " " (pluralize "card" selected-hq) " from HQ"))
                                    (when (and selected-hq selected-rd)
                                      " and")
                                    (when selected-rd
                                      (str " " selected-rd " " (pluralize "card" selected-rd) " from the top of R&D"))))
                      (trash-cards state side eid to-trash {:unpreventable true})))
        choosing-ab (fn [forced-hq]
                      {:waiting-prompt "Corp to choose an option"
                       :player :corp
                       :prompt choosing-prompt
                       :choices {:min forced-hq
                                 :max n
                                 :card #(and (corp? %)
                                             (in-hand? %))}
                       :async true
                       :cancel-effect trash-req
                       :effect trash-req})
        check-forcing-ab {:async true
                          :effect (req
                                    (let [cards-rd (count (:deck corp))
                                          cards-hq (count (:hand corp))
                                          forced-hq (- n cards-rd)]
                                      (if (> forced-hq cards-hq)
                                        (trash-req state :corp eid card (:hand corp))
                                        (resolve-ability state side eid
                                                          (choosing-ab forced-hq)
                                                          card nil))))}]
    {:req (req (> n 0))
     :msg (msg "sabotage " n)
     :async true
     :effect (req (resolve-ability state side check-forcing-ab card targets))}))
