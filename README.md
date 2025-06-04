# Cardfinity
**The TCG with "infinite" possibilities!**

A Trading Card Game where players create the cards, the system does it's best to
ensure decks are fair but it's not perfect. 

There are currently only a few primitive atoms that make up cards but feel free
to make a PR and contribute some more!

You can find some example decks [here](https://github.com/droshux/cardfinity/tree/main/excards), you're also welcome to contribute your own.

## The technical bits

This project is built with `cabal` which can be installed from [`ghcup`](https://www.haskell.org/ghcup/install/#generic_8).

You can build and run the project using `cabal run exes`. 
- `cabal run exes -- dev deckFile`: see information about a deck to help with
deck development.
- `cabal run exes -- pdf deckFile`: create a PDF of a deck, to cut up and play
with in real life.
- `cabal run exes -- play deckFile1 deckFile2`: play the game digitally, the
text interface is awkward but a pretty version may come later (possibly with
[godot-haskell](https://github.com/SimulaVR/godot-haskell)).

## Rules introduction

For full information see [the full rules](/docs/RULES.md), the list of all
"atoms" can be found [here](/docs/ATOMS.md).

The only way to win is by "deckout", if your opponent removes a card from their
deck when the deck is empty you win. This means that your deck is your HP bar.
- Taking damage means moving the top cards of your deck to your graveyard.
- Healing means putting the top cards of your graveyard onto the deck.

Each turn you draw one card and can do any of the following, as many times as
you wish:
- Play a spell card from your hand.
- Play a monster card from your hand onto your field.
- Manually activate a spell of a monster on your field. Often this will "Tap"
the monster, meaning none of the monster's spells can activate until the start
of your next turn.

Monsters can't attack by default, instead "Attack with this monster" is an
effect that must be triggered, this means there is no distinct "battle phase".
