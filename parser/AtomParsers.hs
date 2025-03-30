module AtomParsers (requirement, effect) where

import Atoms
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Natural (Natural)
import ParserCore (CardParser, anyOf, gap, hspace, name, thereIs)
import Text.Megaparsec (MonadParsec (..), between, option, sepBy1)
import Text.Megaparsec.Char (char, string')
import Text.Megaparsec.Char.Lexer (decimal)
import Types (Effect, Requirement)
import Utils (SearchType (..))

requirement :: CardParser Requirement
requirement =
  anyOf
    [ destroy,
      discard,
      takeD,
      healThem,
      pop,
      mbReq
    ]

effect :: CardParser Effect
-- Try all effects but fallback to asEff
effect = anyOf es <|> asEff
  where
    es =
      [ destroyTheir,
        discardTheir,
        dealD,
        healMe,
        deckoutParse,
        drawParse,
        peekParse,
        scryParse,
        chooseParse,
        attackParse,
        playCardEffectParse,
        searchParse,
        attachParse,
        mbEff,
        buff
      ]

searchType :: CardParser SearchType
searchType =
  anyOf
    [ string' "card" $> ForCard,
      ForFamily <$> (try (string' "family") <|> string' "f" >> hspace >> name) <* mbcard,
      ForName <$> name,
      string' "spell" $> ForSpell <* mbcard,
      string' "monster" $> ForMonster <* mbcard
    ]
  where
    mbcard = option () $ hspace <* string' "card"

findCards :: CardParser FindCards
findCards = do
  n <- decimal
  hspace
  typ <- searchType
  hspace
  anyOf
    [ FindCardsField n typ <$ string' "field",
      FindCardsHand n typ <$ string' "hand"
    ]

destroyType :: CardParser DestroyType
destroyType = string' "discard" $> Discard <|> string' "banish" $> Banish

asEff :: CardParser Effect
asEff = asEffect <$> requirement

destroy :: CardParser Requirement
destroy = do
  typ <- destroyType
  hspace
  destroyCards typ <$> findCards

destroyTheir :: CardParser Effect
destroyTheir = do
  typ <- destroyType
  hspace
  void $ string' "enemy"
  hspace
  destroyTheirCards typ <$> findCards

discard :: CardParser Requirement
discard = string' "discard" $> discardDeck

discardTheir :: CardParser Effect
discardTheir = do
  string' "discard" *> hspace
  string' "enemy" $> discardTheirDeck

tortrue :: CardParser Bool
tortrue = thereIs $ try (hspace >> string' "true") <|> string' "t"

takeD :: CardParser Requirement
takeD = do
  string' "take" *> hspace
  n <- decimal
  takeDamage n <$> tortrue

dealD :: CardParser Effect
dealD = do
  string' "deal" *> hspace
  n <- decimal
  dealDamage n <$> tortrue

helper :: CardParser a -> String -> (a -> Effect) -> CardParser Effect
helper d s a = do
  string' s *> hspace
  a <$> d

natHelper :: String -> (Natural -> Effect) -> CardParser Effect
natHelper = helper decimal

healMe :: CardParser Effect
healMe = natHelper "heal" heal

healThem :: CardParser Requirement
healThem = do
  string' "heal" *> hspace
  string' "enemy" *> hspace
  healOpponent <$> decimal

deckoutParse :: CardParser Effect
deckoutParse = string' "deckout" $> deckoutEffect

drawParse :: CardParser Effect
drawParse = natHelper "draw" drawEffect

peekParse :: CardParser Effect
peekParse = natHelper "peek" peek

scryParse :: CardParser Effect
scryParse = natHelper "scry" scry

pop :: CardParser Requirement
pop = do
  string' "pop" *> hspace
  popGraveyard <$> decimal

chooseParse :: CardParser Effect
chooseParse = do
  (e : es) <- between (char '(') (char ')') $ effect `sepBy1` gap
  return $ choose (e :| es)

attackParse :: CardParser Effect
attackParse = do
  string' "attack" *> hspace
  d <- thereIs $ string' "directly"
  return $ attack d

searchParse :: CardParser Effect
searchParse = try (h "search" SearchFor) <|> h "drill" DrillFor
  where
    h s m = search . m <$> (string' s *> hspace *> searchType)

attachParse :: CardParser Effect
attachParse = helper searchType "attach" attach

mbEff :: CardParser Effect
mbEff = youMay <$> (char '?' *> effect)

mbReq :: CardParser Requirement
mbReq = reqYouMay <$> (char '?' *> requirement)

playCardEffectParse :: CardParser Effect
playCardEffectParse = helper searchType "play" playCardEffect

buff :: CardParser Effect
buff = do
  string' "buff" *> hspace
  self <- thereIs $ string' "this" *> hspace
  neg <- thereIs $ char '-'
  power <- decimal
  return $ alterPower (if neg then -power else power) self
