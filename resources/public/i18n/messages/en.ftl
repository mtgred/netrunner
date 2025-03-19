# templates???

# "$side uses $card to trash $cards to gain $credits and draw $cards"

## Helpers
join-with-and = {" "}and{" "}
join-with-comma = ,{" "}

## Ability fragments

use-card = {$username} uses {$title} to {$do-ability}.
pay-use-card = {$username} {$payment} to use {$title} to {$do-ability}.

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

## Standalone messages

increase-trace-strength = {$username} {$payment} to increase trace strength to {$value}.

## Payments 

paid-click = spends {$paid-value} [Click]

paid-credit = pays {$paid-value} [Credit]

paid-credit-pool = pays {$paid-value} [Credit] from credit pool

paid-hosted-credit = pays {$paid-value} [Credit] from {$title}

paid-bad-publicity = pays {$paid-value} [Credit] from bad publicity
