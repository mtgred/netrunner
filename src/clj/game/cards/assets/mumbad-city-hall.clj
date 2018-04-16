(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-mumbad-city-hall
  {"Mumbad City Hall"
   {:abilities [{:label "Search R&D for an Alliance card"
                 :cost [:click 1]
                 :prompt "Choose an Alliance card to play or install"
                 :choices (req (cancellable (filter #(and (has-subtype? % "Alliance")
                                                          (if (is-type? % "Operation")
                                                            (<= (:cost %) (:credit corp)) true)) (:deck corp)) :sorted))
                 :msg (msg "reveal " (:title target) " from R&D and "
                           (if (= (:type target) "Operation") "play " "install ") " it")
                 :effect (req (shuffle! state side :deck)
                              (if (= (:type target) "Operation")
                                (play-instant state side target)
                                (corp-install state side target nil nil)))}]}})
