(in-ns 'game.cards.programs)

(def card-definition-rook
  {"Rook"
   {:abilities [{:cost [:click 1]
                 :label "Host on a piece of ice"
                 :effect (req (let [r (get-card state card)
                                    hosted? (ice? (:host r))
                                    icepos (ice-index state (get-card state (:host r)))]
                                (resolve-ability
                                  state side
                                  {:prompt (if hosted?
                                             (str "Host Rook on a piece of ICE protecting this server or at position "
                                                  icepos " of a different server")
                                             "Host Rook on a piece of ICE protecting any server")
                                   :choices {:req #(if hosted?
                                                     (and (or (= (:zone %)
                                                                 (:zone (:host r)))
                                                              (= (ice-index state %)
                                                                 icepos))
                                                          (= :ices
                                                             (last (:zone %)))
                                                          (ice? %)
                                                          (can-host? %)
                                                          (not (some (fn [c] (has-subtype? "Caïssa"))
                                                                     (:hosted %))))
                                                     (and (ice? %)
                                                          (can-host? %)
                                                          (= :ices
                                                             (last (:zone %)))
                                                          (not (some (fn [c] (has-subtype? "Caïssa"))
                                                                     (:hosted %)))))}
                                   :msg (msg "host it on " (card-str state target))
                                   :effect (effect (host target card))}
                                  card nil)))}]
    :events {:pre-rez-cost {:req (req (= (:zone (:host card))
                                         (:zone target)))
                            :effect (effect (rez-cost-bonus 2))}}}})
