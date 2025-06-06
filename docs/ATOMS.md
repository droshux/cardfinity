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
`on discard`. This requirement is written as: `discard` and has scale `-5`.

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

You select the given number of cards to discard/banish from the opponent's
field/hand. When destroying cards in your opponent's hand you see their hand and
can choose freely which cards to destroy. If you are unable to select enough
cards, the selected cards are destroyed and the spell continues to resolve.

The scale of this effect is the same as the scale of #destroy but positive
instead of negative.

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
of the opponent's deck.

### Choose

Written as a comma separated list of effects, surrounded by brackets (eg: `(attack, banish enemy field)`). The scale is the largest of the scales of each effect in the list. 

When this effect occurs you choose one (and only one) of the effects in the list
to occur.
