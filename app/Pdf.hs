module Pdf (document, pageDimension) where

import Control.Monad (forM, forM_)
import Data.Text (pack)
import GHC.Float (int2Double)
import Graphics.PDF
import Types (Card, cardName)

newtype CardText = CardText AnyFont

instance ComparableStyle CardText where
  isSameStyleAs (CardText f1) (CardText f2) = f1 == f2

instance Style CardText where
  textStyle (CardText f) = TextStyle (PDFFont f 12) black white FillAndStrokeText 1.0 1.0 1.0 1.0

data Vert = VERT

instance ComparableStyle Vert where
  isSameStyleAs VERT VERT = True

instance ParagraphStyle Vert CardText

str :: (Style s) => String -> Para s ()
str = txt . pack

drawCard :: AnyFont -> Rectangle -> Card -> Draw ()
drawCard f pos c = displayFormattedText pos VERT (CardText f) $ paragraph $ do
  str $ cardName c
  return ()

pageDimension :: PDFRect
pageDimension = PDFRect 0 0 595.28 841.89 -- A4

-- Size of card: 180 :+ 252
cardRects :: [Rectangle]
cardRects = flip map [0 :: Int ..] $ \i ->
  let x = 12.64 + 185 * int2Double (i `mod` 3)
      y = 35.445 + 257 * int2Double (i `div` 3)
   in Rectangle (x :+ y) ((x + 180) :+ (y + 252))

pageContent :: [Card] -> PDFReference PDFPage -> PDF ()
pageContent cs = flip drawWithPage $ do
  strokeColor black
  setWidth 0.5
  let rects = take (length cs) cardRects
  forM_ rects stroke

-- 9 playing cards can fit on an A4 page
document :: [Card] -> PDF ()
document cards = do
  let cc = length cards
  let pageCount = (cc `div` 9) + if cc `mod` 9 == 0 then 1 else 0
  forM_ [0 .. pageCount] $ \i ->
    addPage Nothing >>= pageContent (takeCards i cards)
  where
    takeCards i = take 9 . drop (i * 9)
