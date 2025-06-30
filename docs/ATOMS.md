# Atoms

The rules and details of the #requirements and #effects that make up cards.

## Filters

Some atoms use a filter to select specific cards (eg: "Search the deck for a
..."). There are a few possible filters:

- `"[name]"`: Accepts any card with a specified name. Be careful of typos
because the name must match exactly. The name of a monster card is the name of
the monster, the name of a spell card is the name of the spell.
- `family "[family name]"`: Accepts any card that has a specified family. Like
filtering by name, the family must match exactly. A card can have any number of
families (including zero). Can be written as `f"[family name]"` for short.
- `monster`: Accepts any monster card
- `spell`: Accepts any spell card
- `card`: Accepts any card, no filtering occurs.

Examples:
- `"Blue Eyes White Dragon"`
- `family "Pirate"`
- `f"Vampire"`

### Rarity

The scale of a filter depends on the number of copies cards in your deck that
the filter accepts. The scale of a filter is called it's "Rarity" and decreases
by 1 every time the number of accepted copies doubles.

| Rarity      | Scale | Number of copies |
|-------------|-------|------------------|
| Legendary   | 5     | 1                |
| Very Rare   | 4     | 2                |
| Rare        | 3     | 3-4              |
| Uncommon    | 2     | 5-8              |
| Common      | 1     | 9-16             |
| Very Common | 0     | 17+              |

## Requirements

### Destroy

The most complicated and versatile requirement. It is made up of the following
parts:

1. Method: Whether to Discard or Banish the cards. This *does*
   [trigger](/docs/RULES.md#trigger) `on discard`.
2. Count: How many cards to destroy.
3. [Filter](#filters): What cards can be destroyed.
4. Location: Whether to destroy cards in the hand or on the field.

You must select the given number of cards from the given location to
discard/banish, a card cannot destroy itself. If you are unable to destroy the
required number of cards you do not meet the requirement.

`[discard/banish] [count] [filter] [hand/field]`

The method and count form a multiplier which is multiplied by the count, then
the scale of the filter is subtracted.

| Method  | Location | Multiplier |
|---------|----------|------------|
| Discard | Hand     | -10        |
| Banish  | Hand     | -12        |
| Discard | Field    | -15        |
| Banish  | Field    | -17        |

For example: `discard 2 card field -> -15 × 2 - 0`

If the count or filter are omitted they are assumed to be `1` and `card`.
`discard monster field -> discard 1 monster field`
`banish 2 hand -> banish 2 card hand`

### Discard

Discard the top card of your deck, this *does* [trigger](/docs/RULES.md#trigger)
`on discard`. This requirement is written as: `discard` and has scale `-4`.

The difference between "Discard the top card of your deck" and "take one damage"
is that this requirement triggers `on discard` spells. This is important to
avoid exploits such as: `"Decanter of Endless Water" on discard: heal 1`,
allowing you to have unlimited budget by discarding enormous numbers of cards
from your deck without actually losing any cards.

### Take Damage

`take [count] [true?]`

Remove the given number of cards from the top of your deck. If the damage is
"true" cards are banished, otherwise they are placed on top of the graveyard in
the order they were removed from the deck. If there are not enough cards in your
deck [you lose the game](/docs/RULES.md#deckout-and-damage).

This **does not** [trigger](/docs/RULES.md#trigger) `on discard`.

`[count] true` can be written as `[count]t` if desired, note the lack of space
between the count and `t`.

This has scale `-5 × count` for normal damage and `-7 × count` for true damage.

Examples:
- `take 1`: Move the top card of your deck to your graveyard. (`-5`)
- `take 3 true`: Banish the top three cards of your deck. (`-7 × 3 = -21`)
- `take 5`: `take 1` but repeated 5 times. (`-5 × 5 = -25`)
- `take 2t`: Banish the top two cards of your deck. (`-7 × 2 = -14`)

### Heal Opponent

Written as `heal enemy [count]`, identical to #heal but with negative scale
instead of positive, it acts on your opponent instead of you and if there are
not enough cards in the opponent's graveyard to heal you fail to meet the
requirement.

### Pop

Written as `pop [count]` with scale `-2 × count`. Banish the given number of
cards from the top of your graveyard. If there are not enough cards in your
graveyard you fail to meet the requirement. This means that the number of cards
in your graveyard is a resource to be spent that increases as you take more
damage.

[Why is this called "pop"?](https://www.geeksforgeeks.org/stack-data-structure/)

### Choose (Requirement)

Written as a comma separated list of requirements, surrounded by brackets (eg:
`(take 7, discard 3 field, discard 4 hand)`). The scale of this requirement is
the largest (least negative) of the scales of each requirement. In order to meet
this requirement you must choose one of the requirements from the list and meet
that requirement. If you fail to meet the chosen requirement you fail to meet
this requirement.

## Effects

Any requirement can be also used as an effect, if you fail to meet the
requirement the spells continues to resolve. Effects with less than `-5` scale are
treated as having `-5` scale.

### Destroy Target

Non-combat removal, structured the same as #destroy.

`[discard/banish] enemy [count] [filter] [hand/field]`

When destroying from the hand: up to `count` random cards that match the filter
are discarded/banished from the enemy's hand.

When destroying from the field: you select the given number of cards to
discard/banish from the opponent's field. 

If there are not enough cards that match the filter, the cards that can be
destroyed are destroyed and the spell continues to resolve.

The scale of this effect is the same as the scale of #destroy but positive
instead of negative and destroying a card on the enemy field costs en extra `2`.
For example:
- `discard enemy hand`: 10
- `discard enemy field`: 17
- `banish enemy field`: 19

### Discard Enemy

Discard the top card of the opponent's deck. This effect is written as `discard
enemy` and has scale `5`. This does [trigger](/docs/RULES.md#trigger) `on
discard`. To see why this is separate from [deal
damage](/docs/ATOMS.md#deal-damage), see #discard.

### Deal Damage

Written as `deal [count] [true?]`. The same as [take
damage](/docs/ATOMS.md#take-damage) but removes from your opponent's deck
instead of your deck and has positive scale instead of negative.

### Heal

Written as `heal [count]` and has scale `7 × count`. Move the given number of
cards from the top of your graveyard to the top of your deck, in the order they
were added (`on play take 2: heal 2` has no effect on the order of cards in your
deck). Since you can see the contents of your graveyard at any time, you now
know the top cards of your deck. If there are not enough cards in your graveyard
to heal, you heal as many as possible and the spell continues to resolve.

If the count is omitted the default is `1` (`heal -> heal 1`).

### DECKOUT

You lose the game.

Written as `deckout` with scale `-5`.

### Draw

Written as `draw [count]` with scale `10 × count`. Draw the given number of
cards. If you are unable to draw enough cards [you lose the
game](/docs/RULES.md#deckout-and-damage).

If the count is omitted the default is `1` (`draw -> draw 1`).

### Peek

Written as `peak [count]`, see the top cards of the deck. If there are not
enough cards in the deck you see all the cards in the deck and the spell
continues to resolve. If the count is omitted the default is `1` (`peek -> peek
1`)

The scale of this card doubles as the number of cards doubles, the scales for
one to five are shown below:
| # | Scale |
|---|-------|
| 1 | 2     |
| 2 | 4     |
| 3 | 8     | 
| 4 | 16    |
| 5 | 32    |

### Scry

Written as `scry [count]`. Identical to #peek but you instead see the top cards
of the opponent's deck instead of your own.

### Choose

Written as a comma separated list of effects, surrounded by brackets (eg:
`(attack, banish enemy field)`). The scale is the largest of the scales of each
effect in the list plus the number of effects in the list. For example `(draw,
draw 2)` would have scale `22`.

When this effect occurs must you choose one (and only one) of the effects in the
list to occur. 

### Attack

In Cardfinity there is not "battle phase" and by default monsters cannot
initiate combat. Instead the Attack effect allows a monster to initiate combat. 

This effect is written as `attack` and has scale `10`.

#### When can you attack
This effect can only be used on spells who's trigger can only occur on a monster
on the field (eg: you cannot put Attack on a "On Draw" spell).  Since "On Play"
spells on a monster act as "when this monster enters the field", the Attack
effect can be used on "On Play" spells.

If the attack effect occurs and the monster it occurred from is not on the field
nothing happens. For example, in the following scenario no attack would occur:
```
* A copy of "Monster" card is on the field,
a copy of "Spell" is in the hand.*

"Monster":
! This monster attacks after on discard effect of "Spell" has been resolved.
"Choose Violence" on tap discard "Spell" hand: attack

! Therefore by the time "Monster" attacks it is in the graveyard.
"Spell" on discard: discard "Monster" field
```

#### Order of events in combat
Combat takes place as follows:
1. The player that controls the attacking monster chooses a monster from the
   list of the opponents monsters *that are not tapped*. If there are no
non-tapped monsters to choose from the attack is [direct](#direct-attacks).
2. The [power](/docs/RULES.md#monsters) of the two monsters is compared. The
   monster with the most power wins, the attacking monster wins in the case of a
tie. 
3. Any of the losing monster's [On Defeat](/docs/RULES.md#trigger) spells
   trigger.
4. The losing monster is Discarded (triggering any of its On Discard spells).
5. If the attacking monster won any of its [On Victory](/docs/RULES.md#trigger)
   spells trigger.

#### Direct attacks

If there are no valid attack targets, the opponent takes damage equal to the
power of the monster and no defeat or victory spells are triggered. This means
that monsters with high power are often more scale efficient than [the deal
damage effect](#deal-damage). Since tapped monsters do not protect you from
direct attacks, if your opponent has a high power attacking monster you may
want to forgo the "On Tap" spell of a monster and let the attacking monster
destroy it 

#### Piercing attacks

The attack effect can be "Piercing", Piercing Attack is written as `piercing
attack` and has scale `20`. The difference is that if the attacking monster
wins, the opponent takes damage equal to the difference in power. For example if
a power `7` monster attacks a power `4` monster, before the defending monster is
defeated as normal the owner of the power `4` monster would take `3` damage.

### Search

Written as `search [filter]` with scale `30`. However the [filter](#filters) is
`spell` the scale of this effect is `25` instead.

1. Choose any card in your deck that matches the filter and add it to your hand.
2. Shuffle your deck.
3. Trigger any "On Draw" spells on the chosen monster trigger.

