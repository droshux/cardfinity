{-# LANGUAGE OverloadedStrings #-}

module CardView (card) where

import Atoms qualified as CF
import Data.Foldable (Foldable (toList))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set.Ordered qualified as OS
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Optics.Operators ((^.))
import Scale (runScale)
import Shared qualified
import Types qualified as CF

card :: Bool -> [CF.Card] -> CF.Card -> M.View model action
card printMode deck c =
  H.div_
    [ CSS.style_
        [ CSS.aspectRatio "2.5 / 3.5",
          CSS.border "solid 1px black",
          CSS.maxWidth $ if printMode then "2.5in" else CSS.vw 40,
          CSS.maxHeight $ if printMode then "3.5in" else CSS.vh 56,
          CSS.display "flex",
          CSS.flexDirection "column"
        ]
    ]
    [ -- Top Row
      H.div_
        [ CSS.style_
            [ CSS.display "flex"
            ]
        ]
        [ H.p_ [CSS.style_ [CSS.margin $ CSS.em 0.5]] [showScale deck c],
          H.p_
            [ CSS.style_ [CSS.margin $ CSS.em 0.5]
            ]
            [showName c]
        ],
      -- Image
      showImage $ c ^. CF.cardImageUrl,
      H.div_ [CSS.style_ [CSS.display "flex"]] (map (H.em_ [] . (: []) . M.text . M.toMisoString) $ toList $ c ^. CF.cardFamilies),
      case c ^. CF.cardStats of
        CF.SpellStats spell -> showSpell spell
        CF.MonsterStats monster -> showMonster monster
    ]

showName :: CF.Card -> M.View parent action
showName c =
  let name = CF.cardName c
   in if name == "" then H.em_ [] [M.text "No Name"] else M.text $ M.toMisoString name

showScale :: [CF.Card] -> CF.Card -> M.View parent action
showScale deck = M.text . either (const "?") M.toMisoString . runScale deck

showImage :: Maybe String -> M.View parent action
showImage mbUrl =
  H.img_
    [ P.src_ $ maybe "assets/icons/snail.svg" M.toMisoString mbUrl, -- TODO: Replace snail with proper placeholder
      CSS.style_ $ CSS.aspectRatio "2.5 / 2" : [CSS.width "100%" | isNothing mbUrl]
    ]

showSpell :: CF.Spell -> M.View parent action
showSpell s =
  H.div_ [CSS.style_ [CSS.flexGrow 1]] $
    concat
      [ [Shared.triggerIcon (s ^. CF.spellTrigger)],
        spread showCondition (s ^. CF.castingConditions),
        [M.text ": "],
        spread showEffect (s ^. CF.effects)
      ]

showMonster :: CF.Monster -> M.View parent action
showMonster _ = H.div_ [] []

spread f = intercalate [M.text ", "] . map f . toList

-- TODO: "the top 1 card" -> "the top card"
-- TODO: Handle "a" VS "an"

showCondition (CF.Destroy dt fc) = showDestroy False dt fc
showCondition CF.DiscardSelf = piece "Discard the top card of your deck"
showCondition (CF.TakeDamage n b) = map M.text $ ["Take " <> M.toMisoString (show n), " "] ++ ["true " | b] ++ ["damage"]
showCondition (CF.HealOpponent n) = piece $ "Heal the opponent for " <> M.toMisoString (show n) <> " health"
showCondition (CF.Pop n) = map M.text $ ["Banish the top " <> M.toMisoString (show n) <> " card"] ++ ["s" | n /= 1] ++ [" of your graveyard"]
showCondition (CF.YouMay c) = M.text "You may " : showCondition c
showCondition (CF.Choose cs) = [M.text "Choose one:", H.ul_ [] (map (H.li_ [] . showCondition) $ toList cs)]

showEffect :: CF.Effect -> [M.View parent action]
showEffect (CF.DestroyEnemy dt fc) = showDestroy True dt fc
showEffect CF.DiscardEnemy = piece "Discard the top card of the enemy's deck"
showEffect (CF.DealDamage n b) = map M.text $ ["Deal " <> M.toMisoString (show n), " "] ++ ["true " | b] ++ ["damage"]
showEffect (CF.Heal n) = piece $ "Heal " <> M.toMisoString (show n) <> " damage"
showEffect CF.DECKOUT = [H.strong_ [] [M.text "DECKOUT"]]
showEffect (CF.Draw n) = map M.text $ ("Draw " <> M.toMisoString (show n) <> " card") : ["s" | n /= 1]
showEffect (CF.Peek n) = map M.text $ ["See the top " <> M.toMisoString (show n) <> " card"] ++ ["s" | n /= 1] ++ [" of your deck"]
showEffect (CF.Scry n) = map M.text $ ["See the top " <> M.toMisoString (show n) <> " card"] ++ ["s" | n /= 1] ++ [" of the enemy's deck"]
showEffect (CF.Optional e) = M.text "You may " : showEffect e
showEffect (CF.ChooseEffect es) = [M.text "Choose one:", H.ul_ [] (map (H.li_ [] . showEffect) $ toList es)]
showEffect (CF.Attack b) = [H.strong_ [] $ map M.text $ ["Piercing " | b] ++ ["Attack"]]
showEffect (CF.Play st) = M.text "Play a " : showSearchType st
showEffect (CF.Search (CF.SearchFor st)) = M.text "Search the deck for a " : showSearchType st
showEffect (CF.Search (CF.DrillFor st)) = M.text "Drill the deck for a " : showSearchType st
showEffect (CF.Attach st) = [M.text "Attach a "] ++ showSearchType st ++ [M.text " to this monster"]
showEffect (CF.Buff n b) = piece $ (if n < 0 then "Decrease" else "Increase") <> " this monster's power by " <> M.toMisoString (show $ abs n)
showEffect (CF.AsEffect cond) = showCondition cond

showDestroy :: Bool -> CF.DestroyType -> CF.FindCards -> [M.View model action]
showDestroy isEnemy dt fc =
  concat
    [ [ case dt of CF.Discard -> "Discard "; CF.Banish -> "Banish ",
        M.text (M.toMisoString $ show $ CF.getCount fc),
        M.text " "
      ],
      showSearchType (CF.getSearchType fc),
      [M.text "s" | CF.getCount fc /= 1],
      [M.text (if CF.isField fc then " on the " else " in the ")],
      [M.text "enemy's " | isEnemy],
      [M.text (if CF.isField fc then "field" else "hand")]
    ]

showSearchType :: CF.SearchType -> [M.View model action]
showSearchType CF.ForCard = [M.text "card"]
showSearchType CF.ForMonster = [M.text "monster"]
showSearchType CF.ForSpell = [M.text "spell"]
showSearchType (CF.ForName n) = [H.em_ [] [M.text $ M.toMisoString n]]
showSearchType (CF.ForFamily f) = [H.em_ [] [M.text $ M.toMisoString f], M.text " card"]

piece = (: []) . M.text
