module CardParser (card, deck, unparseDeck) where

import AtomParsers (condition, effect)
import Atoms (Condition)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Foldable (find)
import Data.Set.Ordered (OSet, empty, fromList)
import ParserCore
import ShowCard ()
import Text.Megaparsec (MonadParsec (..), choice, manyTill, option, optional, sepBy, sepBy1)
import Text.Megaparsec.Char (char, string')
import Text.Megaparsec.Char.Lexer (decimal)
import Types
  ( Card (..),
    CardStats (MonsterStats, SpellStats),
    DeckInfo (..),
    Display (unparse),
    Monster (..),
    Spell (..),
    Trigger (..),
    cardName,
  )

deck :: CardParser DeckInfo
deck = do
  cardDefs <- manyTill (card <* space) (string' "deck:" *> space)
  (dName, author) <- deckInfo <* space
  cardIncls <- manyTill (cardInclude cardDefs <* space) eof
  return $
    DeckInfo
      { deckName = dName,
        author = author,
        deckList = cardIncls
      }

deckInfo :: CardParser (String, String)
deckInfo = do
  dName <- name <* hspace
  option () $ string' "by" *> hspace
  author <- name
  return (dName, author)

cardInclude :: [Card] -> CardParser (Int, Card)
cardInclude cs = do
  cname <- name
  case find ((==) cname . cardName) cs of
    Nothing -> fail ("Undeclared card name: " ++ cname)
    Just c -> do
      count <- hspace *> option 1 decimal
      return (count, c)

unparseDeck :: Bool -> DeckInfo -> String
unparseDeck concise (DeckInfo {deckName, author, deckList}) =
  let cardDefinition = (\x a -> a ++ "\n\n" ++ x) . unparse concise . snd
      cardInclusion (n, c) a =
        concat
          [ a,
            "\n",
            show (cardName c),
            if n == 1 && concise then "" else " " ++ show n
          ]
   in concat
        [ foldr cardDefinition "" deckList,
          "\n\ndeck:\n",
          show deckName,
          " by ",
          show author,
          foldr cardInclusion "" deckList
        ]

card :: CardParser Card
card = do
  space
  stats <- MonsterStats <$> try monster <|> SpellStats <$> try spell
  space
  f <- families
  Card 0 f stats <$> image

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
      OnAttach <$ string' "attach",
      Infinity <$ string' "infinity",
      Counter <$ string' "counterspell"
    ]

families :: CardParser (OSet String)
families = option Data.Set.Ordered.empty $ fmap fromList $ surround '(' ')' $ name `sepBy` gap

image :: CardParser (Maybe String)
image = option Nothing $ do
  string' "image:" *> hspace
  Just <$> name

spell :: CardParser Spell
spell = do
  n <- name
  space
  t <- trigger
  space
  conds <- conditions
  char ':' *> space
  effects <- effect `sepBy1` gap
  return $ Spell n t conds effects

monster :: CardParser Monster
monster = do
  monsterName <- name <* char ':'
  space
  conds <- conditions
  space
  spells <- concat <$> manyTill (mspells <* space) (string' "power")
  optional (char ':') *> hspace
  power <- decimal
  space
  startsTapped <- option False $ do
    _ <- string' "tapped"
    return True
  return $ Monster monsterName spells conds power False startsTapped
  where
    mspells :: CardParser [Spell]
    mspells = do
      n <- option 1 $ decimal <* (string' "x " <|> string' "x")
      replicate n <$> spell

conditions :: CardParser (OSet Condition)
conditions = fmap fromList $ option [] $ condition `sepBy` gap
