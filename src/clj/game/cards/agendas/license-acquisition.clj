(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-license-acquisition
  {"License Acquisition"
   {:interactive (req true)
    :prompt "Select an asset or upgrade to install from Archives or HQ"
    :show-discard true
    :choices {:req #(and (#{"Asset" "Upgrade"} (:type %))
                         (#{[:hand] [:discard]} (:zone %))
                         (= (:side %) "Corp"))}
    :msg (msg "install and rez " (:title target) ", ignoring all costs")
    :effect (effect (corp-install eid target nil {:install-state :rezzed-no-cost}))}})