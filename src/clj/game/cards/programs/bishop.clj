(in-ns 'game.core)

(declare can-host?)

(def card-programs-bishop
  {"Bishop"
   {:abilities [{:cost [:click 1]
                 :effect (req (let [b (get-card state card)
                                    hosted? (ice? (:host b))
                                    remote? (is-remote? (second (:zone (:host b))))]
                                (resolve-ability state side
                                 {:prompt (msg "Host Bishop on a piece of ICE protecting "
                                            (if hosted? (if remote? "a central" "a remote") "any") " server")
                                  :choices {:req #(if hosted?
                                                    (and (if remote?
                                                           (is-central? (second (:zone %)))
                                                           (is-remote? (second (:zone %))))
                                                         (ice? %)
                                                         (can-host? %)
                                                         (= (last (:zone %)) :ices)
                                                         (not (some (fn [c] (has-subtype? c "Caïssa"))
                                                                    (:hosted %))))
                                                    (and (ice? %)
                                                         (can-host? %)
                                                         (= (last (:zone %)) :ices)
                                                         (not (some (fn [c] (has-subtype? c :subtype "Caïssa"))
                                                                    (:hosted %)))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))} card nil)))}]
    :events {:pre-ice-strength
             {:req (req (and (= (:cid target) (:cid (:host card))) (:rezzed target)))
              :effect (effect (ice-strength-bonus -2 target))}}}})
