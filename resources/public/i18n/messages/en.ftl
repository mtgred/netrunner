### templates???

## Terms aka consistent names

-corp = {$case ->
    [genitive] Corp
    [accusative] the Corp
    *[nominative] the Corp
}

-runner = {$case ->
    [genitive] Runner
    [accusative] the Runner
    *[nominative] the Runner
}

-archives = Archives
-hq = HQ
-rd = R&D

-grip = the grip
-heap = the heap
-stack = the stack

-credit = [Credit]
-click = [Click]
-bad-publicity = bad publicity
-tag = tag

-credit-pool = credit pool

## card types

agenda = agenda
    .plural = agendas
asset = asset
    .plural = assets
event = event
    .plural = events
hardware = hardware
    .plural = pieces of hardware
ice = piece of ice
    .plural = pieces of ice
    .type = ICE
identity = identity
    .plural = identities
operation = operation
    .plural = operations
resource = resource
    .plural = resources
upgrade = upgrade
    .plural = upgrades

## locations

server-name = {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] Server {$server}
}

## Logical templates

use-card = {$username} uses {$title} to {$do-ability}.
pay-use-card = {$username} {$payment} to use {$title} to {$do-ability}.

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}and{" "}
join-list = ,{" "}

## Ability fragments

trash-card = trash {$title}
trash-card-at-no-cost = trash {$title} at no cost

trash-n-cards = trash {$count ->
    [one] 1 card
    *[other] {$count} cards
}

trash-cards = trash {$count ->
    [one] 1 card ({$titles})
    *[other] {$count} cards ({$titles})
}

trash-accessed-card = trash the accessed card ({$title})
trash-all-cards-in-grip = trash all cards in {-grip}

trash-all-agendas-by-type = trash all {agenda.plural}
trash-all-assets-by-type = trash all {asset.plural}
trash-all-hardware-by-type = trash all {hardware.plural}
trash-all-ice-by-type = trash all {ice.type}
trash-all-identities-by-type = trash all {identity.plural}
trash-all-operations-by-type = trash all {operation.plural}
trash-all-resource-by-type = trash all {resource.plural}
trash-all-upgrade-by-type = trash all {upgrade.plural}

gain-credits = gain {$value} {-credit}

draw-cards = draw {$value ->
    [one] 1 card
    *[other] {$value} cards
}

avoid-tags = avoid {$count ->
    [one] 1 tag
    *[other] {$count} tags
}

take-tags = take {$value ->
    [one] 1 tag
    *[other] {$value} tags
}

remove-tags = remove {$value ->
    [one] 1 tag
    *[other] {$value} tags
}

shuffle-grip-into-stack = shuffle { -grip } into { -stack }
shuffle-grip-and-heap-into-stack = shuffle { -grip } and { -heap } into { -stack }
shuffle-self-into-stack = shuffle itself into {-stack}
shuffle-cards-into-stack = shuffle {$count ->
    [zero] 0 cards into {-stack}
    [one] 1 card ({$titles}) into {-stack}
    *[other] {$count} cards ({$titles}) into {-stack}
}
shuffle-stack = shuffle {-stack}

forfeit = forfeits {$title}
add-self-to-score-area = add itself to [their] score area as an agenda worth {$value ->
    [one] 1 agenda point
    *[other] {$value} agenda points
}

give-bad-publicity = give {-corp(case: "accusative")} {$value} { -bad-publicity }

force-trash-installed-ice = force {-corp(case: "accusative")} to trash a {ice} protecting {$server}

add-card-to-grip = add {$title} to [their] {-grip}

add-card-from-stack-to-grip = add {$title} from {-stack} to [their] {-grip}

move-seen-unseen-into-grip = move {$seen} and {$unseen ->
    [one] 1 unseen card into {-grip}
    *[other] {$unseen} unseen cards into {-grip}
}
move-seen-into-grip = move {$seen} into {-grip}
move-unseen-into-grip = move {$unseen-cnt ->
    [one] 1 unseen card into {-grip}
    *[other] {$unseen-cnt} unseen cards into {-grip}
}

move-seen-unseen-into-hq = move {$seen} and {$unseen-cnt ->
    [one] 1 unseen card into {-hq}
    *[other] {$unseen-cnt} unseen cards into {-hq}
}
move-seen-into-hq = move {$seen} into {-hq}
move-unseen-into-hq = move {$unseen-cnt ->
    [one] 1 unseen card into {-hq}
    *[other] {$unseen-cnt} unseen cards into {-hq}
}

expose-card = expose {$title}
reveal-n-cards-in-hq = reveal {$count} cards from {-hq}
reveal-cards-in-hq = reveal {$count} cards ({$titles}) from {-hq}

disable-corp-id = disable {-corp(case:"nominative")} identity
disable-runner-id = disable {-runner(case:"nominative")} identity

do-nothing = do nothing

take-additional-turn = take an additional turn after this one

rearrange-installed-ice = rearrange any number of ice protecting all servers

remove-advancement-counters = remove {$count ->
    [one] 1 advancement counter from {$title}
    *[other] {$count} advancement counters from {$title}
}

look-at-top-cards-add-to-grip = look at the top {$top-count ->
    [one] card of the stack and add {$add-count} of them to the grip
    *[other] {$top-count} cards of the stack and add {$add-count} of them to the grip
}

guess = guess {$choice}

reveal-copies-of-self = reveal {$count ->
    [one] 1 copy of itself
    *[other] {$count} copies of itself
}

force-corp-trash-top-of-rd = force the Corp to trash the top {$count ->
    [one] card of R&D
    *[other] {$count} cards of R&D
}

force-corp-trash-additional-top-of-rd = force the Corp to trash an additional {$count ->
    [one] card from the top of R&D
    *[other] {$count} cards from the top of R&D
}

force-corp-rez = force the Corp to rez {$title}
force-corp-trash = force the Corp to trash {$title}

each-player-draws-cards = make each player draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}

# Runs

make-a-run = make a run
make-a-run-on = make a run on {$server}

run-on-with-no-rezzed-ice = make a run on {$server} during which no ice can be rezzed

bypass-ice = bypass {$title}

# Access

access-another-card = access another card
access-additional-in-hq = access {$count ->
    [one] 1 additional card in {-hq}
    *[other] {$count} additional cards in {-hq}
}
access-additional-in-rd = access {$count ->
    [one] 1 additional card in {-rd}
    *[other] {$count} additional cards in {-rd}
}

## Specific card abilities

# Apocalypse
trash-all-installed-corp = trash all installed { -corp(case: "genitive") } cards

# Apocalypse
turn-all-installed-runner-facedown = turn all installed { -runner(case: "genitive") } cards facedown

## Traces

increase-trace-strength = {$username} {$payment} to increase trace strength to {$value}.

## Turn messages

corp-start-of-turn = {$username} started [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}
corp-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -hq }.
    *[other] {$cards} cards in { -hq }.
}

runner-start-of-turn = {$username} started [their] turn {$turn} with {$credits} {-credit} and {$cards ->
    [one] 1 card in { -grip }.
    *[other] {$cards} cards in { -grip }.
}
runner-end-of-turn = {$username} is ending [their] turn {$turn} with {$credits} {-credit} and {$cards ->
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

## Moving

forfeit-agenda = {$username} forfeits {$title}.

## Payments 

payment-click = spends {$value} {-click}
payment-credit = pays {$value} {-credit}
payment-credit-pool = pays {$value} {-credit} from [their] {-credit-pool}
payment-hosted-credit = pays {$value} {-credit} from {$title}
payment-bad-publicity = pays {$value} {-credit} from {-bad-publicity}

payment-trash-from-grip = reveals and trashes {$count ->
    [one] 1 card ({$titles}) from {-grip}
    *[other] {$count} cards ({$titles}) from {-grip}
}
payment-trash-from-hq = reveals and trashes {$count ->
    [one] 1 card ({$titles}) from {-hq}
    *[other] {$count} cards ({$titles}) from {-hq}
}
