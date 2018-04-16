(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-constellation-protocol
  {"Constellation Protocol"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12
            (req (let [tokens (filter #(pos? (:advance-counter % 0)) (all-installed state :corp))
                       advanceable (filter #(can-be-advanced? %) (all-installed state :corp))]
                   (when (and (not-empty tokens) (not-empty (clojure.set/difference (set advanceable) (set tokens))))
                     true)))}
    :once :per-turn
    :abilities [{:label "Move an advancement token between ICE"
                 :choices {:req #(and (ice? %) (:advance-counter %))}
                 :priority true
                 :effect (req (let [fr target]
                                (resolve-ability
                                  state side
                                  {:priority true
                                   :prompt "Move to where?"
                                   :choices {:req #(and (ice? %)
                                                        (not= (:cid fr) (:cid %))
                                                        (can-be-advanced? %))}
                                   :effect (effect (add-prop :corp target :advance-counter 1)
                                                   (add-prop :corp fr :advance-counter -1)
                                                   (system-msg (str "uses Constellation Protocol to move an advancement token from "
                                                                    (card-str state fr) " to " (card-str state target))))} card nil)
                                card nil))}]}})
