module Pdf (document, pageDimension, documentAlt, pageDimensionAlt, Fonts (..)) where

import Control.Monad (forM_, unless, void, when, zipWithM_)
import Data.Text (pack)
import GHC.Float (int2Double)
import Graphics.PDF
import Optics.Operators
import Types
import Utils (collapse, showFold)

data Fonts = Fonts
  { n :: AnyFont,
    bold :: AnyFont,
    italic :: AnyFont
  }
  deriving (Eq)

data HStyle
  = CardText Fonts
  | NameText Fonts Bool
  | FamilyText Fonts
  | SpellNameText Fonts

instance ComparableStyle HStyle where
  isSameStyleAs (CardText f1) (CardText f2) = f1 == f2
  isSameStyleAs (NameText f1 m1) (NameText f2 m2) = f1 == f2 && m1 == m2
  isSameStyleAs (FamilyText f1) (FamilyText f2) = f1 == f2
  isSameStyleAs (SpellNameText f1) (SpellNameText f2) = f1 == f2
  isSameStyleAs (CardText _) _ = False
  isSameStyleAs (NameText _ _) _ = False
  isSameStyleAs (FamilyText _) _ = False
  isSameStyleAs (SpellNameText _) _ = False

instance Style HStyle where
  textStyle (CardText f) =
    TextStyle
      { textFont = PDFFont (n f) 10,
        textStrokeColor = black, -- Not used
        textFillColor = black,
        textMode = FillText,
        penWidth = 0.0, -- Not used
        scaleSpace = 1.0,
        scaleDilatation = 1.0,
        scaleCompression = 1.0
      }
  textStyle (FamilyText f) =
    (textStyle $ CardText f)
      { textFillColor = Rgb 0.5 0.5 0.5,
        textFont = PDFFont (italic f) 8
      }
  textStyle (NameText f monster) =
    (textStyle $ CardText f)
      { textFont = PDFFont (n f) 12,
        textFillColor = if monster then Rgb 0.8 0.6 0.4 else Rgb 0.4 0.8 0.8
      }
  textStyle (SpellNameText f) =
    (textStyle $ CardText f)
      { textFont = PDFFont (italic f) 10
      }

data VStyle = Header | Stats | SpellPara

instance ComparableStyle VStyle where
  isSameStyleAs Header Header = True
  isSameStyleAs Stats Stats = True
  isSameStyleAs SpellPara SpellPara = True
  isSameStyleAs Header _ = False
  isSameStyleAs Stats _ = False
  isSameStyleAs SpellPara _ = False

instance ParagraphStyle VStyle HStyle where
  linePosition Header _ _ = 0
  linePosition _ _ _ = 5
  lineWidth Header w _ = w
  lineWidth _ w _ = w - 10
  paragraphStyle SpellPara = Just $ \r d -> do
    fillColor $ Rgb 0.9 0.9 0.9
    fill r
    void d
  paragraphStyle _ = Nothing

str :: (Style s) => String -> Para s ()
str = txt . pack

drawCard :: Fonts -> Rectangle -> Card -> Draw ()
drawCard fnt rect crd = displayFormattedText rect Header (CardText fnt) $ do
  setJustification Centered
  glue 5 1.0 1.0
  paragraph $ do
    setStyle $ NameText fnt $ isMonster crd
    str $ cardName crd
  paragraph $ unless (null $ crd ^. cardFamilies) $ do
    setStyle $ FamilyText fnt
    str $ showFold ", " $ crd ^. cardFamilies
  setParaStyle Stats
  setJustification LeftJustification
  writeStats fnt $ crd ^. cardStats

writeStats :: Fonts -> CardStats -> TM VStyle HStyle ()
writeStats fnt (SpellStats s) = writeSpell fnt 1 False s
writeStats fnt (MonsterStats m) = do
  setStyle $ CardText fnt
  paragraph $ str $ showFold ", " $ m ^. summoningConditions
  mapM_ showSpell $ collapse $ m ^. monsterSpells
  paragraph $ do
    str $ "Power: " ++ show (m ^. combatPower)
    when (m ^. isTapped) $ forceNewLine >> str "Begins Tapped"
  where
    showSpell (spl, n) = do
      -- when (i > 1) $ str $ show i ++ "x "
      writeSpell fnt n True spl
      glue 5 1.0 2.0

writeSpell :: Fonts -> Int -> Bool -> Spell -> TM VStyle HStyle ()
writeSpell fnt n showName s = do
  setLinePenalty 5
  setParaStyle SpellPara
  paragraph $ do
    when showName $ do
      setStyle $ SpellNameText fnt
      str $ show $ s ^. spellName
    setStyle $ CardText fnt
    when (n > 1) $ str $ " " ++ show n ++ "x"
    str " "
    str $ show $ s ^. spellTrigger
    str " "
    str $ showFold ", " $ s ^. castingConditions
    str ": "
    str $ showFold ", " $ s ^. effects
    endPara
  setParaStyle Stats

pageDimension :: PDFRect
pageDimension = PDFRect 0 0 595.28 841.89 -- A4

pageDimensionAlt :: PDFRect
pageDimensionAlt = PDFRect 0 0 841.89 595.28 -- A4 Landscape

-- Size of card: 180 :+ 252
cardRects :: [Rectangle]
cardRects = flip map [0 :: Int ..] $ \i ->
  let x = 10.14 + 187.5 * int2Double (i `mod` 3)
      y = 25.445 + 262 * int2Double (i `div` 3)
   in Rectangle (x :+ y) ((x + 180) :+ (y + 252))

cardRectsAlt :: [Rectangle]
cardRectsAlt = flip map [0 :: Int ..] $ \i ->
  let x = 25.445 + 262 * int2Double (i `mod` 3)
      y = 10.14 + 187.5 * int2Double (i `div` 3)
   in Rectangle (x :+ y) ((x + 252) :+ (y + 180))

pageContent :: [Rectangle] -> Fonts -> [Card] -> PDFReference PDFPage -> PDF ()
pageContent rectGen fnt cs = flip drawWithPage $ do
  strokeColor black
  setWidth 0.5
  let rects = take (length cs) rectGen
  forM_ rects stroke
  zipWithM_ (drawCard fnt) rects cs

divideCards :: [Rectangle] -> Fonts -> [Card] -> PDF ()
divideCards rectGen fnt cards = do
  let cc = length cards
  let pageCount = (cc `div` 9) + if cc `mod` 9 == 0 then 1 else 0
  forM_ [0 .. pageCount] $ \i ->
    addPage Nothing >>= pageContent rectGen fnt (takeCards i cards)
  where
    takeCards i = take 9 . drop (i * 9)

-- 9 playing cards can fit on an A4 page
document :: Fonts -> [Card] -> PDF ()
document = divideCards cardRects

documentAlt :: Fonts -> [Card] -> PDF ()
documentAlt = divideCards cardRectsAlt
