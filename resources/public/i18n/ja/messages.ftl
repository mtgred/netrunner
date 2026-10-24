## Logical templates

use-card = {$username} uses {$title} to {$do-ability}.
pay-use-card = {$username} {$payment} to use {$title} to {$do-ability}.

# join ability framents together naturally
# for example, "$player uses $card to do x and do y and do z" will use [join-with-and]
# like this: "$player uses $card to do x[join-with-and]do y[join-with-and]do z."

join-with-and = {" "}and{" "}
join-list = ,{" "}

## locations

server-name = {$server ->
    [archives] {-archives}
    [hq] {-hq}
    [rd] {-rd}
    *[other] Server {$server}
}

## Ability fragments

trash-card = trash {$card-str}
trash-card-at-no-cost = trash {$card-str} at no cost

trash-n-cards = trash {$count ->
    [zero] no cards
    [one] 1 card
    *[other] {$count} cards
}

trash-cards = trash {$count ->
    [zero] no cards
    [one] 1 card ({$card-strs})
    *[other] {$count} cards ({$card-strs})
}

gain-credits = gain {$count} {-credit}

draw-cards = draw {$count ->
    [one] 1 card
    *[other] {$count} cards
}

make-a-run = make a run
make-a-run-on = make a run on {$server}

## Payments

payment-click = spends {$value} {-click}
payment-credit = pays {$value} {-credit}
