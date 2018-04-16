(in-ns 'game.core)

(declare can-host?)

(def card-programs-pawn
  {"Pawn"
   {:implementation "All abilities are manual"
    :abilities [{:label "Host Pawn on the outermost ICE of a central server"
                 :prompt "Host Pawn on the outermost ICE of a central server" :cost [:click 1]
                 :choices {:req #(and (ice? %)
                                      (can-host? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Advance to next ICE"
                 :prompt "Choose the next innermost ICE to host Pawn on it"
                 :choices {:req #(and (ice? %)
                                      (can-host? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Trash Pawn and install a Caïssa from your Grip or Heap, ignoring all costs"
                 :effect (req (let [this-pawn (:cid card)]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a Caïssa program to install from your Grip or Heap"
                                   :show-discard true
                                   :choices {:req #(and (has-subtype? % "Caïssa")
                                                        (not= (:cid %) this-pawn)
                                                        (#{[:hand] [:discard]} (:zone %)))}
                                   :msg (msg "install " (:title target))
                                   :effect (effect (runner-install target {:no-cost true}))} card nil)
                                (trash state side card)))}]}})