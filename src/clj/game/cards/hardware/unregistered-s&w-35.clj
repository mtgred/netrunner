(in-ns 'game.core)

(def card-hardware-unregistered-s&w-35
  {"Unregistered S&W 35"
   {:abilities
    [{:cost [:click 2]
      :req (req (some #{:hq} (:successful-run runner-reg)))
      :label "trash a Bioroid, Clone, Executive or Sysop"
      :prompt "Select a Bioroid, Clone, Executive, or Sysop to trash"
      :choices {:req #(and (rezzed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop"))
                           (or (and (= (last (:zone %)) :content) (is-remote? (second (:zone %))))
                               (= (last (:zone %)) :onhost)))}
      :msg (msg "trash " (:title target)) :effect (effect (trash target))}]}})
