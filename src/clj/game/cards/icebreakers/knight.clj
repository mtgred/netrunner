(in-ns 'game.core)

(def card-definitions-icebreakers-knight
  {"Knight"
   {:abilities [{:label "Host Knight on a piece of ICE"
                 :effect (req (let [k (get-card state card)
                                    hosted (ice? (:host k))
                                    icepos (ice-index state (get-card state (:host k)))]
                                (resolve-ability state side
                                 {:prompt (msg "Host Knight on a piece of ICE" (when hosted " not before or after the current host ICE"))
                                  :cost [:click 1]
                                  :choices {:req #(if hosted
                                                    (and (or (when (= (:zone %) (:zone (:host k)))
                                                               (not= 1 (abs (- (ice-index state %) icepos))))
                                                             (not= (:zone %) (:zone (:host k))))
                                                         (ice? %)
                                                         (can-host? %)
                                                         (installed? %)
                                                         (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))
                                                    (and (ice? %)
                                                         (installed? %)
                                                         (can-host? %)
                                                         (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %)))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))} card nil)))}
                {:cost [:credit 2]
                 :req (req (ice? (get-nested-host card)))
                 :msg "break 1 subroutine on the host ICE"}]}})
