module AtomParser where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Natural (Natural)
import ParserCore (CardParser, anyOf, gap, hspace, name, thereIs)
import Text.Megaparsec (MonadParsec (..), between, option, sepBy1)
import Text.Megaparsec.Char (char, string')
import Text.Megaparsec.Char.Lexer (decimal)
import Atoms (Effect(..),Condition (..),SearchType (..),FindCards(..),DestroyType (..),SearchMethod(..))

condition :: CardParser Condition
condition =
  anyOf
    [ destroy,
      discard,
      takeD,
      healThem,
      pop,
      mbCond,
      rchooseParse
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
  option ForCard $
    anyOf
      [ string' "card" $> ForCard,
        string' "spell" $> ForSpell,
        string' "monster" $> ForMonster,
        ForFamily <$> (try (string' "family") <|> string' "f" >> hspace >> name),
        ForName <$> name
      ]

findCards :: CardParser FindCards
findCards = do
  n <- option 1 decimal
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
asEff = AsEffect <$> condition

destroy :: CardParser Condition
destroy = do
  typ <- destroyType
  hspace
  Destroy typ <$> findCards

destroyTheir :: CardParser Effect
destroyTheir = do
  typ <- destroyType
  hspace
  void $ string' "enemy"
  hspace
  DestroyEnemy typ <$> findCards

discard :: CardParser Condition
discard = string' "discard" $> DiscardSelf

discardTheir :: CardParser Effect
discardTheir = do
  string' "discard" *> hspace
  string' "enemy" $> DiscardEnemy

tortrue :: CardParser Bool
tortrue = thereIs $ try (hspace >> string' "true") <|> string' "t"

takeD :: CardParser Condition
takeD = do
  string' "take" *> hspace
  n <- decimal
  TakeDamage n <$> tortrue

dealD :: CardParser Effect
dealD = do
  string' "deal" *> hspace
  n <- decimal
  DealDamage n <$> tortrue

helper :: CardParser a -> a -> String -> (a -> Effect) -> CardParser Effect
helper d def s a = do
  string' s *> hspace
  a <$> option def d

natHelper :: String -> (Natural -> Effect) -> CardParser Effect
natHelper = helper decimal 1

healMe :: CardParser Effect
healMe = natHelper "heal" Heal

healThem :: CardParser Condition
healThem = do
  string' "heal" *> hspace
  string' "enemy" *> hspace
  HealOpponent <$> decimal

deckoutParse :: CardParser Effect
deckoutParse = string' "deckout" $> DECKOUT

drawParse :: CardParser Effect
drawParse = natHelper "draw" Draw

peekParse :: CardParser Effect
peekParse = natHelper "peek" Peek

scryParse :: CardParser Effect
scryParse = natHelper "scry" Scry

pop :: CardParser Condition
pop = do
  string' "pop" *> hspace
  Pop <$> decimal

chooseParse :: CardParser Effect
chooseParse = do
  (e : es) <- between (char '(' *> hspace) (hspace <* char ')') $ effect `sepBy1` gap
  return $ ChooseEffect (e :| es)

rchooseParse :: CardParser Condition
rchooseParse = do
  (r : rs) <- between (char '(' *> hspace) (hspace <* char ')') $ condition `sepBy1` gap
  return $ Choose (r :| rs)

attackParse :: CardParser Effect
attackParse = do
  p <- thereIs $ string' "piercing" <* hspace
  void $ string' "attack"
  return $ Attack p

searchParse :: CardParser Effect
searchParse = try (h "search" SearchFor) <|> h "drill" DrillFor
  where
    h s m = Search . m <$> (string' s *> hspace *> searchType)

attachParse :: CardParser Effect
attachParse = helper searchType ForSpell "attach" Attach

mbEff :: CardParser Effect
mbEff = Optional <$> (char '?' *> effect)

mbCond :: CardParser Condition
mbCond = YouMay <$> (char '?' *> condition)

playCardEffectParse :: CardParser Effect
playCardEffectParse = helper searchType ForSpell "play" Play

buff :: CardParser Effect
buff = do
  string' "buff" *> hspace
  self <- thereIs $ string' "this" *> hspace
  neg <- thereIs $ char '-'
  power <- decimal
  return $ Buff (if neg then -power else power) self
