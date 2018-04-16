(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-salvaged-vanadis-armory
  {"Salvaged Vanadis Armory"
   {:events {:damage
             {:effect (req (show-wait-prompt state :corp "Runner to use Salvaged Vanadis Armory")
                           (resolve-ability state :runner
                                            {:optional
                                             {:prompt "Use Salvaged Vanadis Armory?"
                                              :yes-ability {:msg (msg "force the Corp to trash the top " (get-turn-damage state :runner) " cards of R&D and trash itself")
                                                            :effect (effect (mill :corp (get-turn-damage state :runner))
                                                                            (clear-wait-prompt :corp)
                                                                            (trash card {:unpreventable true}))}
                                              :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                                            card nil))}}}})