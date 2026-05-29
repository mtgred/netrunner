### templates???

## Terms aka consistent names

-corp = {$case ->
    [nominative] Corpo
    *[adjective] la Corpo
}
-runner = {$case ->
    [nominative] Runner
    *[adjective] lae Runner
}

-archives = Archives
-hq = QG
-rd = R&D

-grip = la main
-heap = le tas
-stack = la pile

## Logical templates

use-card = {$username} utilise {$title} pour {$do-ability}.
pay-use-card = {$username} {$payment} pour utiliser {$title} pour {$do-ability}.

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}et{" "}

## Ability fragments

trash-card = efface {$title}

trash-cards = efface {$value ->
    [one] 1 carte
    *[other] {$value} cartes
}

gain-credits = gagner {$value} [Credit]

draw-cards = piocher {$value ->
    [one] 1 carte
    *[other] {$value} cartes
}

avoid-tags = éviter {$value ->
    [one] 1 tag
    *[other] {$value} tags
}

take-tags = prend {$value ->
    [one] 1 tag
    *[other] {$value} tags
}

shuffle-grip-into-stack = mélange { -grip } dans { -stack }
shuffle-grip-and-heap-into-stack = mélange { -grip } et { -heap } dans { -stack }

## Specific card abilities

# Apocalypse
trash-all-installed-corp = effacer toutes les cartes { -corp(case: "nominative") } installées

# Apocalypse
turn-all-installed-runner-facedown = retourner toutes les cartes { -runner(case: "nominative") } installées face cachée

## Standalone messages

increase-trace-strength = {$username} {$payment} pour augmenter la force de la trace à {$value}.

## Payments 

payment-click = dépense {$value} [Click]
payment-credit = paie {$value} [Credit]
payment-credit-pool = paie {$value} [Credit] depuis sa réserve de crédits.
payment-hosted-credit = paie {$value} [Credit] depuis {$title}
payment-bad-publicity = paie {$value} [Credit] de Mauvaise Presse
