module ParserCore where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, between, choice, option, someTill)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as Mega
import Text.Megaparsec.Char.Lexer qualified as MegaLex

type CardParser = Parsec Void String

mkspace :: CardParser () -> CardParser ()
mkspace s = MegaLex.space s (MegaLex.skipLineComment "!") (MegaLex.skipBlockComment "*" "*")

space :: CardParser ()
space = mkspace Mega.space1

hspace :: CardParser ()
hspace = mkspace Mega.hspace1

newline :: CardParser ()
newline = void $ hspace *> (Mega.newline <|> Mega.char ';')

gap :: CardParser ()
gap = void $ Mega.char ',' *> hspace

name :: CardParser String
name = Mega.char '"' *> someTill MegaLex.charLiteral (Mega.char '"')

surround :: Char -> Char -> CardParser a -> CardParser a
surround c1 c2 = between (char c1 *> hspace) (hspace <* char c2)

thereIs :: CardParser a -> CardParser Bool
thereIs a = option False $ a $> True

anyOf :: [CardParser a] -> CardParser a
anyOf = choice . map try
