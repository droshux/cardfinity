module Editor.Update (update) where
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import Data.Set.Ordered (OSet, fromList, (<|))
import Editor.Types
import Miso qualified as M
import Miso.Lens (Lens, lens, (%=), (%~), (.=), (.~), (^.))
type Effect parent = M.Effect parent DeckModel DeckAction
type SubUpdate a m parent = Lens DeckModel m -> a -> Effect parent

listHelper ::
  (Default m) =>
  Lens DeckModel [m] ->
  SubUpdate a m parent ->
  ListAction a ->
  Effect parent
listHelper l u NewItem = l %= (def :)
listHelper l u (Delete i) = l %= replace i Nothing
listHelper l u (ItemAction i a) = u (focus l i) a
  where
    focus l i =
      let get m = (m ^. l) !! i; set x = l %~ replace i (Just x)
       in lens get (flip set)

osetHelper ::
  (Default m, Ord m) =>
  Lens DeckModel (OSet m) ->
  SubUpdate a m parent ->
  ListAction a ->
  Effect parent
osetHelper l u NewItem = l %= (def <|)
osetHelper l u (Delete i) = l %= (fromList . replace i Nothing . toList)
osetHelper l u (ItemAction i a) = u (focus l i) a
  where
    focus l i =
      let get m = toList (m ^. l) !! i
          set x = l %~ fromList . replace i (Just x) . toList
       in lens get (flip set)

nelistHelper ::
  (Default m) =>
  Lens DeckModel (NonEmpty m) ->
  SubUpdate a m parent ->
  ListAction a ->
  Effect parent
nelistHelper l u NewItem = l %= cons def
nelistHelper l u (Delete i) = l %= replaceNE i Nothing
nelistHelper l u (ItemAction i a) = u (focus l i) a
  where
    focus l i =
      let get m = case (i, m ^. l) of
            (0, x :| _) -> x
            (i, _ :| tail) -> tail !! (i - 1)
          set x = l %~ replaceNE i (Just x)
       in lens get (flip set)

replace i mx xs =
  let x' = case mx of Just x -> [x]; Nothing -> []
   in take i xs ++ x' ++ drop (i + 1) xs

replaceNE 0 Nothing xs@(_ :| []) = xs -- Illegal Operation => Noop
replaceNE 0 Nothing (_ :| (h : tail)) = h :| tail
replaceNE 0 (Just x) (_ :| tail) = x :| tail
replaceNE i mx (h :| tail) = h :| replace (i - 1) mx tail

(%) :: Lens a b -> Lens b c -> Lens a c
(%) f g =
  let get = (^. g) . (^. f)
      set a c = (f .~ (g .~ c) (a ^. f)) a
   in lens get set
