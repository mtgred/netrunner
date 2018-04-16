(in-ns 'game.core)

(def card-hardware-the-gauntlet
  {"The Gauntlet"
   {:implementation "Requires Runner to manually (and honestly) set how many ICE were broken directly protecting HQ"
    :in-play [:memory 2]
    :events {:post-successful-run {:req (req (and (= :hq target)
                                                  run))
                                   :silent (req true)
                                   :delayed-completion true
                                   :effect (effect (continue-ability
                                                     {:prompt "How many ICE protecting HQ did you break all subroutines on?"
                                                      ;; Makes number of ice on server (HQ) the upper limit.
                                                      ;; This should work since trashed ice do not count according to UFAQ
                                                      :choices {:number (req (count (get-in @state [:corp :servers :hq :ices])))}
                                                      :effect (effect (access-bonus target))}
                                                     card nil))}}}})