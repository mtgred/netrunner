(in-ns 'game.core)

(def card-definitions-operations-shoot-the-moon
  {"Shoot the Moon"
   {:choices {:req #(and (ice? %) (not (rezzed? %)))
              :max (req (min (:tag runner)
                             (reduce (fn [c server]
                                       (+ c (count (filter #(not (:rezzed %)) (:ices server)))))
                                     0 (flatten (seq (:servers corp))))))}
    :req (req tagged)
    :effect (req (doseq [t targets] (rez state side t {:ignore-cost :all-costs}))
                 (effect-completed state side eid card))}})
