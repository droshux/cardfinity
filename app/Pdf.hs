module Pdf where
import Graphics.PDF 
import Data.Text (pack)
import Types (Card, cardName)

newtype CardText = CardText AnyFont
instance ComparableStyle CardText where
    isSameStyleAs (CardText f1) (CardText f2) = f1 == f2
instance Style CardText where
    textStyle (CardText f) = TextStyle (PDFFont f 12) black white FillAndStrokeText 1.0 1.0 1.0 1.0
data Vert = VERT
instance ComparableStyle Vert where
    isSameStyleAs VERT VERT = True

instance ParagraphStyle Vert CardText where

str :: (Style s) => String -> Para s ()
str = txt . pack

drawCard :: AnyFont -> Rectangle -> Card -> Draw ()
drawCard f pos c = displayFormattedText pos VERT (CardText f) $ paragraph $ do
    str $ cardName c
    return ()
