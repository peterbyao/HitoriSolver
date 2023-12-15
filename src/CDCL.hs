module CDCL ( findSat
                , Var
                , Lit
                , Clause
                , Formula ) where
{-

 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 CDCL implementation is based on 

-}

import Control.Monad
import Control.Applicative
import Control.Conditional (ifM)
import Control.Monad.State
import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.IntMap as M
import Data.Bifunctor (second)

type Var = Int

type Lit = Int

type Clause = [Lit]

type Formula = [Clause]

data Maplet = Maplet { value :: !Bool
                     , reason :: !(Maybe Clause)
                     }

type Assignment = M.IntMap Maplet

type AlgState = (Formula, Assignment)

newtype AlgMonad x = AlgMonad { runMonad :: forall r. AlgState -> (AlgState -> x -> r) -> (Formula -> r) -> r }

instance Monad AlgMonad where
    xm >>= f = AlgMonad $ \s@(_, _) k kf -> runMonad xm s (\(!s') (!x) -> runMonad (f x) s' k kf) kf

instance MonadPlus AlgMonad where
    mzero = AlgMonad $ \(!f, _) _ kf -> kf f

    xm `mplus` ym = AlgMonad $ \s@(_, a) k kf ->
        runMonad xm s k (\(!f') -> runMonad ym (f', a) k kf)

instance MonadState AlgState AlgMonad where
    get = AlgMonad $ \(!s) k _ -> k s s
    put s = AlgMonad $ \_ k _ -> k s ()

instance Functor AlgMonad where
    fmap :: (a -> b) -> AlgMonad a -> AlgMonad b
    fmap = liftM

instance Applicative AlgMonad where
    (<*>) :: AlgMonad (a -> b) -> AlgMonad a -> AlgMonad b
    (<*>) = ap
    pure x = AlgMonad $ \(!s) k _ -> k s x

instance Alternative AlgMonad where
    empty = mzero
    (<|>) = mplus

choices :: Assignment -> [Lit]
choices !a = map interpret $ M.assocs a where
    interpret (v, mp) | value mp = v
                      | otherwise = -v

varOfLit :: Lit -> Var
varOfLit !l = abs l

polarityOfLit :: Lit -> Bool
polarityOfLit !l = l > 0

addMaplet :: Lit -> Maybe Clause -> Assignment -> Assignment
addMaplet !l !r !a = M.insert v m a where
    v = varOfLit l
    m = Maplet { value = polarityOfLit l
               , reason = r
               }

isMentioned :: Lit -> Assignment -> Bool
isMentioned !l !a = isJust $ M.lookup (varOfLit l) a

lookupLit :: Lit -> Assignment -> Maybe Bool
lookupLit !l !a = (==polarityOfLit l) . value <$> M.lookup (varOfLit l) a

clauseAfterAssignment :: Assignment -> Clause -> Maybe Clause
clauseAfterAssignment !a !c
    | isSat = Nothing
    | otherwise = Just simplified where
        isSat = or $ mapMaybe (`lookupLit` a) c
        simplified = filter (not . (`isMentioned` a)) c

formulaAfterAssignment :: Formula -> Assignment -> Formula
formulaAfterAssignment !f !a = mapMaybe (clauseAfterAssignment a) f

unitPropagation :: AlgState -> AlgState
unitPropagation (!f, !a) = (f, a') where
    lits = nubBy (\(x, _) (y, _) -> varOfLit x == varOfLit y)
         $ map filterX
         $ filter ((==1) . length . fst)
         $ mapMaybe (\c -> do x <- clauseAfterAssignment a c; return (x, c)) f
    filterX ([x], y) = (x, Just $ filter (/=x) y)
    filterX _ = error "Unit Propagation Error"
    a' = foldr (uncurry addMaplet) a lits

addClause :: Clause -> AlgState -> AlgState
addClause !c (!f, !a) = (c:f, a)

chooseLit :: AlgState -> Lit
chooseLit (!f, !a) = head
                   $ minimumBy (comparing length)
                   $ formulaAfterAssignment f a

learnedClauses :: AlgState -> [Clause]
learnedClauses (!f, !a) = map getConflict
                        $ take 1
                        $ filter ((==Just []) . clauseAfterAssignment a) f where
    getConflict [] = []
    getConflict (c:cs) = case M.lookup (varOfLit c) a >>= reason of
        Just r -> getConflict $ r ++ cs
        Nothing -> c : getConflict cs

satisfied :: AlgState -> Bool
satisfied (!f, !a) = null (formulaAfterAssignment f a)

unsatisfied :: AlgState -> Bool
unsatisfied (!f, !a) = elem [] $ formulaAfterAssignment f a

hasUnit :: AlgState -> Bool
hasUnit (!f, !a) = any ((==1) . length) $ formulaAfterAssignment f a

algorithmAction :: AlgMonad [Lit]
algorithmAction = go where
    go = get >>= mainCond

    mainCond s
        | satisfied s = gets (choices . snd)
        | unsatisfied s = learnAndFail
        | hasUnit s = unitProp
        | otherwise = tryLiteral

    unitProp = do modify' unitPropagation ; go

    learnAndFail = do
        cs <- gets learnedClauses
        forM_ cs $ \c -> unless (null c) $ modify $ addClause c
        mzero

    tryLiteral = gets chooseLit >>= exhaust

    choose lit = do modify' $ Data.Bifunctor.second (addMaplet lit Nothing) ; go

    exhaust lit = choose lit `mplus` ifM (gets $ not . unsatisfied) (choose (-lit)) mzero

findSat :: Formula -> Maybe [Lit]
findSat f = runMonad algorithmAction (f, M.empty) (\_ x -> Just x) (const Nothing)