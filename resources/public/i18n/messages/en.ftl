### templates???

## Terms aka consistent names

-corp = {$case ->
    [nominative] Corp
    *[adjective] the Corp
}
-runner = {$case ->
    [nominative] Runner
    *[adjective] the Runner
}

-archives = Archives
-hq = HQ
-rd = R&D

-grip = the grip
-heap = the heap
-stack = the stack

## Logical templates

use-card = {$username} uses {$title} to {$do-ability}.
pay-use-card = {$username} {$payment} to use {$title} to {$do-ability}.

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}and{" "}

## Ability fragments

trash-card = trash {$title}

trash-cards = trash {$value ->
    [one] 1 card
    *[other] {$value} cards
}

gain-credits = gain {$value} [Credit]

draw-cards = draw {$value ->
    [one] 1 card
    *[other] {$value} cards
}

avoid-tags = avoid {$value ->
    [one] 1 tag
    *[other] {$value} tags
}

take-tags = take {$value ->
    [one] 1 tag
    *[other] {$value} tags
}

shuffle-grip-into-stack = shuffle { -grip } into { -stack }
shuffle-grip-and-heap-into-stack = shuffle { -grip } and { -heap } into { -stack }

## Specific card abilities

# Apocalypse
trash-all-installed-corp = trash all installed { -corp(case: "nominative") } cards

# Apocalypse
turn-all-installed-runner-facedown = turn all installed { -runner(case: "nominative") } cards facedown

## Traces

increase-trace-strength = {$username} {$payment} to increase trace strength to {$value}.

## Turn messages

# (str pre " [their] turn " (:turn @state) " with " credits " [Credit] and " (quantify cards "card") " in " hand)
corp-start-of-turn = {$username} started [their] turn {$turn} with {$credits} [Credit] and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}
corp-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} [Credit] and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}.

runner-start-of-turn = {$username} started [their] turn {$turn} with {$credits} [Credit] and {$cards ->
    [one] 1 card in { -grip }.
    *[other] {$cards} cards in { -grip }.
}
runner-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} [Credit] and {$cards ->
    [one] 1 card in { -grip }.
    *[other] {$cards} cards in { -grip }.
}

mandatory-start-of-turn-draw = {$username} makes [their] mandatory start of turn draw.

no-further-actions = {$username} has no further actions.

skip-discard-step = {$username} skips [their] discard step this turn.

corp-discard-cards-from-hand-eot = {$username} discards {$cards ->
    [one] 1 card from { -hq } at end of turn.
    *[other] {$cards} from { -hq } at end of turn.
}
runner-discard-cards-from-hand-eot = {$username} discards {$cards} from {-stack} at end of turn.

extra-turns-remaining = {$username} will have {$turns ->
    [one] 1 extra turn remaining.
    *[other] {$turns} extra turns remaning.
}

## Game End

tie = The game has ended in a tie.
win = {$username} wins the game.
concede = {$username} concedes.
win-decked = {$username} is decked.
win-flatline = {$username} is flatlined.
clear-win = {$username} cleared the win condition.

## Mulligan

mulligan-take = {$username} takes a mulligan.
mulligan-keep = {$username} keeps [their] hand.

## Payments 

payment-click = spends {$value} [Click]
payment-credit = pays {$value} [Credit]
payment-credit-pool = pays {$value} [Credit] from credit pool
payment-hosted-credit = pays {$value} [Credit] from {$title}
payment-bad-publicity = pays {$value} [Credit] from bad publicity
