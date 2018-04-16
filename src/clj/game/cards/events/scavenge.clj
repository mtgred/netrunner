(in-ns 'game.core)

(declare run-event)

(def card-events-scavenge
  {"Scavenge"
   {:prompt "Select an installed program to trash"
    :choices {:req #(and (is-type? % "Program")
                         (installed? %))}
    :effect (req (let [trashed target tcost (- (:cost trashed)) st state si side]
                   (trash state side trashed)
                   (resolve-ability
                     state side
                     {:prompt "Select a program to install from your Grip or Heap"
                      :show-discard true
                      :choices {:req #(and (is-type? % "Program")
                                           (#{[:hand] [:discard]} (:zone %))
                                           (can-pay? st si nil (modified-install-cost st si % [:credit tcost])))}
                      :effect (effect (install-cost-bonus [:credit (- (:cost trashed))])
                                      (runner-install target))
                      :msg (msg "trash " (:title trashed) " and install " (:title target))} card nil)))}})