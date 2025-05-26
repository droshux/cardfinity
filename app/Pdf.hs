module Pdf (document, pageDimension) where

import Control.Monad (forM_, void, when, zipWithM_)
import Data.Text (pack)
import GHC.Float (int2Double)
import Graphics.PDF
import Optics.Operators
import Types
import Utils (showFold)

data HStyle
  = CardText AnyFont
  | NameText AnyFont Bool
  | FamilyText AnyFont

instance ComparableStyle HStyle where
  isSameStyleAs (CardText f1) (CardText f2) = f1 == f2
  isSameStyleAs (NameText f1 m1) (NameText f2 m2) = f1 == f2 && m1 == m2
  isSameStyleAs (FamilyText f1) (FamilyText f2) = f1 == f2
  isSameStyleAs (CardText _) _ = False
  isSameStyleAs (NameText _ _) _ = False
  isSameStyleAs (FamilyText _) _ = False

instance Style HStyle where
  textStyle (CardText f) =
    TextStyle
      { textFont = PDFFont f 8,
        textStrokeColor = black,
        textFillColor = black,
        textMode = FillText,
        penWidth = 0.1,
        scaleSpace = 1.0,
        scaleDilatation = 1.0,
        scaleCompression = 1.0
      }
  textStyle (FamilyText f) =
    (textStyle $ CardText f)
      { textFillColor = Rgb 0.5 0.5 0.5
      }
  textStyle (NameText f monster) =
    (textStyle $ CardText f)
      { textFont = PDFFont f 14,
        textFillColor = if monster then Rgb 0.8 0.6 0.4 else Rgb 0.4 0.8 0.8,
        penWidth = 0.5
      }

data VStyle = Default | Stats | SpellList

instance ComparableStyle VStyle where
  isSameStyleAs Default Default = True
  isSameStyleAs Stats Stats = True
  isSameStyleAs SpellList SpellList = True
  isSameStyleAs Default _ = False
  isSameStyleAs Stats _ = False
  isSameStyleAs SpellList _ = False

instance ParagraphStyle VStyle HStyle where
  linePosition Default _ _ = 0
  linePosition _ _ _ = 5
  lineWidth Stats w _ = w - 10
  lineWidth _ w _ = w
  paragraphStyle SpellList = Just $ \(Rectangle bl (x' :+ y')) x -> do
    void x
    setWidth 0.1
    strokeColor $ Rgb 0.2 0.2 0.2
    stroke $ Rectangle bl $ (x' - 10) :+ y'
  paragraphStyle _ = Nothing

str :: (Style s) => String -> Para s ()
str = txt . pack

drawCard :: AnyFont -> Rectangle -> Card -> Draw ()
-- TODO: Use drawTextBox instead of displayFormattedText?!
drawCard fnt rect crd = displayFormattedText rect Default (CardText fnt) $ do
  setJustification Centered
  paragraph $ do
    setStyle $ NameText fnt $ isMonster crd
    str $ cardName crd
    forceNewLine
    setStyle $ FamilyText fnt
    str $ showFold ", " $ crd ^. cardFamilies
  setJustification LeftJustification
  setParaStyle Stats
  writeStats fnt $ crd ^. cardStats

writeStats :: AnyFont -> CardStats -> TM VStyle HStyle ()
writeStats fnt (SpellStats s) = writeSpell fnt s
writeStats fnt (MonsterStats m) = do
  setStyle $ CardText fnt
  paragraph $ str $ showFold ", " $ m ^. summoningConditions
  setParaStyle SpellList
  mapM_ showSpell $ m ^. monsterSpells
  setParaStyle Stats
  paragraph $ do
    str $ "Power: " ++ show (m ^. combatPower)
    when (m ^. isTapped) $ forceNewLine >> str "Begins Tapped"
  where
    showSpell spl = do
      writeSpell fnt spl
      glue 10 2.0 1.0

writeSpell :: AnyFont -> Spell -> TM VStyle HStyle ()
writeSpell fnt s = do
  setLinePenalty 0
  paragraph $ do
    setStyle $ CardText fnt
    str $ show s
    endPara

pageDimension :: PDFRect
pageDimension = PDFRect 0 0 595.28 841.89 -- A4

-- Size of card: 180 :+ 252
cardRects :: [Rectangle]
cardRects = flip map [0 :: Int ..] $ \i ->
  let x = 12.64 + 185 * int2Double (i `mod` 3)
      y = 35.445 + 257 * int2Double (i `div` 3)
   in Rectangle (x :+ y) ((x + 180) :+ (y + 252))

pageContent :: AnyFont -> [Card] -> PDFReference PDFPage -> PDF ()
pageContent fnt cs = flip drawWithPage $ do
  strokeColor black
  setWidth 0.5
  let rects = take (length cs) cardRects
  forM_ rects stroke
  zipWithM_ (drawCard fnt) rects cs

-- 9 playing cards can fit on an A4 page
document :: AnyFont -> [Card] -> PDF ()
document fnt cards = do
  let cc = length cards
  let pageCount = (cc `div` 9) + if cc `mod` 9 == 0 then 1 else 0
  forM_ [0 .. pageCount] $ \i ->
    addPage Nothing >>= pageContent fnt (takeCards i cards)
  where
    takeCards i = take 9 . drop (i * 9)
