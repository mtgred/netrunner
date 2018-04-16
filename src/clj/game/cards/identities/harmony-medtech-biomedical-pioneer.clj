(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-harmony-medtech-biomedical-pioneer
  {"Harmony Medtech: Biomedical Pioneer"
   {:effect (effect (lose :agenda-point-req 1) (lose :runner :agenda-point-req 1))
    :leave-play (effect (gain :agenda-point-req 1) (gain :runner :agenda-point-req 1))}})
