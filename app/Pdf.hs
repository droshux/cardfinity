module Pdf (testDoc) where

import Data.Text (pack)
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

testPageContent :: PDFReference PDFPage -> PDF ()
testPageContent page = drawWithPage page $ do
  strokeColor red
  setWidth 0.5
  stroke $ Rectangle (10 :+ 0) (200 :+ 300)

testDoc :: PDF ()
testDoc = do
  page1 <- addPage Nothing
  testPageContent page1
