module Parser
  ( CardParser,
    card,
    space,
    hspace,
    name,
    gap,
  )
where

import Atoms (discardDeck, drawEffect)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Data.Set.Ordered (OSet, empty, fromList)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, manyTill, option, optional, sepBy, sepBy1, someTill, (<?>))
import Text.Megaparsec.Char (char, string')
import Text.Megaparsec.Char qualified as Mega
import Text.Megaparsec.Char.Lexer (charLiteral, decimal, nonIndented, skipBlockComment, skipLineComment)
import Text.Megaparsec.Char.Lexer qualified as MegaLex
import Types (Card (..), CardStats (..), Effect, Monster (..), Requirement, Spell (..), Trigger (..))

type CardParser = Parsec Void String

mkspace :: CardParser () -> CardParser ()
mkspace s = MegaLex.space s (skipLineComment "!") (skipBlockComment "*" "*")

space :: CardParser ()
space = mkspace Mega.space1

hspace :: CardParser ()
hspace = mkspace Mega.hspace1

newline :: CardParser ()
newline = void $ hspace *> (Mega.newline <|> char ';')

card :: CardParser Card
card = do
  let n = nonIndented hspace
  stats <- MonsterStats <$> n monster <|> SpellStats <$> n spell
  f <- families
  return
    Card
      { _cardStats = stats,
        _cardID = 0,
        _cardFamilies = f
      }

trigger :: CardParser Trigger
trigger = do
  void $ optional (string' "on" *> hspace)
  choice
    [ OnPlay <$ string' "play",
      OnDraw <$ string' "draw",
      OnDiscard <$ string' "discard",
      OnDefeat <$ string' "defeat",
      OnVictory <$ string' "victory",
      OnTap <$ string' "tap",
      Infinity <$ string' "infinity"
    ]

gap :: CardParser ()
gap = void $ char ',' *> hspace

name :: CardParser String
name = char '"' *> someTill charLiteral (char '"')

families :: CardParser (OSet String)
families = fromList <$> name `sepBy` gap

spell :: CardParser Spell
spell = do
  n <- name
  space
  t <- trigger
  space
  reqs <- requirements
  char ':' *> space
  effects <- effect `sepBy1` gap
  return
    Spell
      { _spellTrigger = t,
        _spellName = n,
        _effects = effects,
        _castingConditions = reqs
      }

monster :: CardParser Monster
monster = do
  monsterName <- name <* char ':'
  space
  reqs <- requirements
  space
  spells <- manyTill (spell <* space) (string' "power")
  optional (char ':') *> hspace
  power <- decimal
  space
  startsTapped <- option False $ newline <* string' "tapped" >> pure True
  return
    Monster
      { _summoningConditions = reqs,
        _monsterSpells = spells,
        _monsterName = monsterName,
        _isTapped = startsTapped,
        _combatPower = power
      }

requirement :: CardParser Requirement
requirement = string' "discard" $> discardDeck

requirements :: CardParser (OSet Requirement)
requirements = option empty $ fromList <$> requirement `sepBy` (char ',' *> hspace)

effect :: CardParser Effect
effect = string' "draw" $> drawEffect 1
