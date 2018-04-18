(in-ns 'game.core)

(def card-definitions-agendas-breaking-news
  {"Breaking News"
   {:delayed-completion true
    :effect (effect (tag-runner :runner eid 2))
    :silent (req true)
    :msg "give the Runner 2 tags"
    :end-turn {:effect (effect (lose :runner :tag 2))
               :msg "make the Runner lose 2 tags"}}})
