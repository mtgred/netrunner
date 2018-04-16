(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-museum-of-history
  {"Museum of History"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (pos? (count (get-in @state [:corp :discard]))))}
    :abilities [{:label "Shuffle cards in Archives into R&D"
                 :prompt (msg (let [mus (count (filter #(and (= "10019" (:code %)) (rezzed? %)) (all-installed state :corp)))]
                                (str "Select " (if (< 1 mus) (str mus " cards") "a card")
                                     " in Archives to shuffle into R&D")))
                 :choices {:req #(and (card-is? % :side :corp) (= (:zone %) [:discard]))
                           :max (req (count (filter #(and (= "10019" (:code %)) (rezzed? %)) (all-installed state :corp))))}
                 :show-discard true
                 :priority 1
                 :once :per-turn
                 :once-key :museum-of-history
                 :msg (msg "shuffle "
                           (let [seen (filter :seen targets)
                                 n (count (filter #(not (:seen %)) targets))]
                             (str (join ", " (map :title seen))
                                  (when (pos? n)
                                    (str (when-not (empty? seen) " and ")
                                         (quantify n "card")))))
                           " into R&D")
                 :effect (req (doseq [c targets] (move state side c :deck))
                              (shuffle! state side :deck))}]}})