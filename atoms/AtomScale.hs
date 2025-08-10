module Atoms where 
import Scale (HasScale)

instance HasScale Condition where
    scale (Destroy d f) = do
        let multiplier = (if isField f then 15 else 10) + (if d == Banish then 2 else 0)
        let n = natToInt (getCount f)
        st <- scale (getSearchType f)
        return $ -(n * multiplier + st)
    scale DiscardSelf = return $ -4
    scale (TakeDamage n isTrue) = return $ natToInt n * (if isTrue then -7 else -5)
    scale (HealOpponent n) = scale (heal n) <&> (\x -> -x)
    scale (Pop n) = return $ -(2 * natToInt n)
    scale (YouMay cond) = scale cond <&> (+ 2)
    scale (Choose cs) = mapM scale (NonE.toList cs) <&> maximum


instance HasScale Effect where
    scale (DestroyEnemy d f) = local (\c -> c {ignoreSTNotFound = True}) (scale (Destroy d f)) <&> (\x -> -x + if isField f then 2 else 0)
    scale DiscardEnemy = scale (Discard::Condition) <&> (3 -)
    scale (DealDamage n isTrue) = scale (TakeDamage n t) <&> (3-)
    scale (Heal n) =return $ 7 * natToInt n 
    scale DECKOUT = return $ -punishment
    scale (Draw n) = return $ natToInt n * 10
    scale (Peek n) = return $ 2 ^ n
    scale (Scry n) = scale (Peek n)
    scale (Optional e) = scale e <&> max (-punishment)
    scale (ChooseEffect es) = mapM scale (NonE.toList es) <&> (+ length es) . maximum
    scale (Attack piercing) = return $ if piercing then 20 else 10
    scale (Play t) = case t of
        ForSpell -> return $ -3
        o -> scale o
    scale (Search (SearchFor ForSpell)) = return 25
    scale (Search (SearchFor _)) = return 30
    scale (Search (DrillFor t)) = local (\c -> c {ignoreSTNotFound = True}) (scale t) <&> (+ 10)
    scale (Attach t) = scale_ t <&> (+ 5)
    scale (Buff by forItself) = return $ max (-punishment) $ if forItself then 2 * fromIntegral by else 3 * fromIntegral by
    scale (AssEffect cond) = scale cond

instance HasScale SearchType where
  scale t = asks deckContext <&> length . filter (toPredicate t) >>= calcRarity
    where
      calcRarity :: Int -> Scale
      calcRarity x
        | x == 0 = do
            ignore <- asks ignoreSTNotFound
            unless ignore $ throwError $ SearchTypeNotFound $ show t
            return 0
        -- Halfing the number of copies -> increase rarity by 1
        | x == 1 = return 5
        | x == 2 = return 4
        | x >= 3 && x <= 4 = return 3
        | x >= 5 && x <= 8 = return 2
        | x >= 9 && x <= 16 = return 1
        | otherwise = return 0

