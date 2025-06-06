# Cardfinity Rulebook

This game is for two players, each player has a deck of 40 to 60 cards. There
are two types of cards, "spell cards" and "monster cards". 

Each card has a list of "families", tags that can be used to [select groups of
cards](/docs/ATOMS.md#filters). Families are written surrounded by quotes, in a
comma separated list. If there are no families `N/A` must be written. For
example: `"Beast", "Machine"` or `N/A`. The families are written after the rest
of the card definition.

There are four important locations for each player:
1. The Deck: your [health](#deckout-and-damage) and resources. At the start of
   each turn you put the top card of your deck into your hand. The deck is kept
face down making it hidden information, but there are ways to see the top few
cards. 
2. The Hand: Your immediately available resources, hidden from the other player.
   The player that takes the first turn begins with four cards (and immediately
draws a fifth) and the other player begins with five in their hand. 
3. The Field: Your currently active monsters, publicly available information.
   Monsters on the field provide a variety of functions, including protecting
yourself and re-usable resources.
4. The Graveyard: Where spent and discarded cards go. Restoring cards to your
   deck is difficult but possible. The number cards in your graveyard is also a
resource in itself.

There are two ways to destroy a card:
1. **[Discard](#trigger)**: Move the card to the graveyard.
1. **Banish**: Remove the card from play entirely. Banished cards cannot be
   restored.

### Comments

When defining a deck, typing a `!` will mark the rest of the line as a comment,
useful for adding notes and reminders to your deck for anyone who reads it. `!
Make sure "Ice Heart" is ready in your hand`. You can also create a
multi-line comment using `*`. It's important to describe how best to use your
deck and why any strange seeming choices were made, especially if you plan to
share it with friends (or bitter cardfinity rivals).
```
*This is the primary boss of the deck, you can search it with "Book of
Abremelan" or "Crowley"'s "Boleskin" spell. Once you do have this out, focus
your "Purple Potion" on this to bump its power up*
```

## Deckout and Damage

There is no "Life Points" in Cardfinity, instead you lose when you attempt to
remove a card from the deck when there are no cards remaining ("deckout"). This
means that drawing cards "damages" you and drawing at the start of each turn
slowly brings you closer to defeat. Note that even with no cards in your deck
all is not lost, you only deckout when you are forced to draw/discard/take
damage when there are no cards in the deck. 

This means that "take damage" means: move the top card of your deck to the
graveyard, taking more than one damage is simply repeating this process. There
is a second kind of damage though, "true" damage, instead of moving cards to
your graveyard cards banished. One of the implications is that taking damage
also impacts the resources available to you as useful cards in your deck become
difficult/impossible to access.

## Scale

The "scale" of a card is the system's measure of how balanced a card is, the
more a card can do the higher the scale and the harder a card is to use the
lower the scale. 

The scale of a card cannot be more than 10 and the total scale of all cards in
your deck cannot be more than 200. Cards with negative scale do not count to
this total though. The scale of a spell that is part of a monster may be up to
15 but the total scale of a monster must still be 10 or less.

## Spells

A spell card is a single spell, monster cards are made up of multiple spells so
it is best to start here. Spells that are part of monster cards are called
"monster spells".

A spell has four parts: 
1. The name of the spell.
1. The [trigger](#trigger), how or when the spell is cast.
1. The [casting requirements](#requirements), what you must do to cast the
   spell.
1. The [spell effects](#effects), what happens when the spell is cast.

The scale of a spell is the sum of the scales of all these parts (excluding the
name).

Spells are written as follows: `"[name]" [trigger] [requirements]: [effects]`,
note the `:` used to separate requirements from effects.

Examples of spells:
- `"Stimulants" play pop 3: buff 3`
- `"Dark Cycle" on tap discard hand: draw 1`
- `"Pus Explosion" discard ?take 1, heal enemy 1: discard enemy field`

### Trigger

- `on play`: Spells that are activated from your hand in some way.
  - On a spell card: the spell can be played from your hand on your turn. Once
  the spell is resolved, you put the card onto your graveyard.
  - On a "monster spell": the spell is activated immediately after the monster
  enters the field.
- `on discard`: This spell automatically activates when this card is discarded.
Either from hand, field or deck. Note that "damage" moves cards from the deck to
the graveyard but does not "discard" meaning this trigger is not activated.
- `on draw`: When this card is enters your hand from the deck this spell is
automatically activated.

The following triggers are "monster only", meaning that they can only be
activated when part of a monster card. They may still be used on spell cards but
cannot be activated.

- `on infinity`: You can activate this spell as many times as you want on your turn
but only if it is on the field. 
- `on tap`: You can activate this spell on your turn if it is on the field. When
the spell resolves the monster becomes "tapped". No spells can be activated on
tapped monsters.
- `on attach`: when this spell is added to the list of spells on a monster, this
spell activates automatically.
- `on victory`:  Automatically triggered after this monster attacks another monster
that has equal or lower "power".
- `on defeat`: Automatically triggered after this monster attacks another monster
that has more "power" than it. This triggers before this monster is discarded.

The scale of `on play` and `on attach` is 0, the scale of `on infinity` is 50 and all
other triggers have scale 5.

The `on` can be omitted (eg: `play`, `infinity`).

### Requirements

After a spell is activated, you must meet each one of it's requirements in order
before the effects can occur. If you fail to meet any of the requirements the
spell does not activate at all.

All requirements must be different and the scale of the requirements is just the
total of the scales of each requirement. The scale of a requirement is (almost
always) negative.

For example: if a spell has "Discard 1 card from the hand, discard 1 card from
the field", if you have at least one card in your hand but no cards on the field
the effects do not occur. Since you failed to meet the requirements, the spell
was never triggered meaning that you get to keep the card you discarded trying
to meet the requirements. If the card discarded has a `on discard` spell the
consequences of that spell are also erased. It is as if the spell was never
activated. 

Details on each requirement can be found [here](/docs/ATOMS.md#requirements).

Requirements are written in a comma separated list, for example: `banish monster
field, banish spell hand, pop 1`

### Effects

After a spell has been activated and all the requirements have been met, the
effects of the spell trigger in turn. Unlike requirements, if an effect cannot
be triggered the spell is not cancelled. 

The effects of a spell do not have to be unique and the scale of the effects are
the total of the scales of each effect, plus 5 for each effect beyond the first.
Spells with many effects can become quite expensive. If an effect has less than
`-5` scale, it is instead treated as if it had `-5` scale. For example: if a spell
had three effects with scales `2, 3, -7, -1` the scale contribution of the effects
would be: `2 + 3 - 5 - 1 + 5 + 5 + 5`.

Details on each effect can be found [here](/docs/ATOMS.md#effects).

Effects are written in a comma separated list, for example: `attack, banish
enemy field`

## Monsters

A monster has four parts:
1. The name of the monster
1. The summoning #requirements of the monster. You must meet
these to play the monster from your hand onto the field.
1. A list of #spells. These spells can have scale up to 15 but the
total scale of the monster must still be 10 or less.
1. The monster's "power". A number that represents the combat capability of the
monster. Since monsters cannot attack by default, the monster's power normally
represents the sturdiness or self defence ability of the monster.

The scale of a monster is the sum of the scale of all these parts (excluding the
name). A monster may enter the game tapped, meaning that none of it's spells can
trigger until it first begins your turn on the field, this reduces the scale of
the monster by 5.

The scale of the list of spells is the total of the scales of each spell plus 10
for each spell beyond the first, monsters with many spells can become very
expensive very quickly.

The scale of the monster's power is the power value multiplied by the number of
digits. For example: a power of 5 has scale 5, a power of 12 has scale 24 and a
power of 100 would have a scale of 300. This means that increasing the power of
a monster brings diminishing returns. However it also means that most monsters
will have less than 10 power unless intentionally built to be overpowering,
forming a separate paradigm of unstoppable forces and immovable objects.

Monsters are written as follows:
`"[name]": [requirements] [spells] power: [power] [tapped?]`, although often the
different parts of a monster are separated onto multiple lines. Note the `:`
after the name and `power`.

### Example Monsters
```
"Justice": discard field
"Persona" play: ?attach
power: 9
```

Monsters can have any number of spells:
```
"Wheel Demon":
banish monster field, banish spell hand, pop 1
"Accelerate" tap: attack
"Momentum" victory take 1: ?attack
power: 5
```

```
! An example of a monster all on one line, tough to read right?
"Door Head Ant": take 1 power 9
```
