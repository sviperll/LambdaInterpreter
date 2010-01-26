{-
    Copyright 2009 Victor Nazarov

    This file is part of LambdaInterpreter.

    LambdaInterpreter is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    LambdaInterpreter is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with LambdaInterpreter.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE FlexibleContexts #-}

module Lambda.Eval
  ( nf
  , whnf
  , nfM
  , nfMZ
  , whnfM
  , whnfMZ
  , eliminateEta
  , backSubstitution
  , toBasisMZ
  , toBasisM
  , toBasis
  , ToBasisData (..)) where

import Lambda.Common
import Lambda.Zipper

import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.List as List
import Data.Set as Set

-- import Debug.Trace

{-
Basic evaluator is evaluator to Weak Head Normal Form.
It transforms term until it gets the form of
\v. E
or
v E1 E2 ... En
where v is some variable, and E, E1, E2, ..., En are custom terms.
The main rule of transformation is beta-reduction:
(\x. N) M -> N[x := M]
Where N[x := M] is the same as N except all free occurances of x
are replaced with M:

This is the Zipper version. It takes zipper and return zipper
pointing to the position where evaluator stops...

mmm... this type annotations are impossible in Haskell 98 and
steel hard to write in GHC
-}


whnfMZ :: (MonadWriter [(String, Zipper)] m, MonadReader (Map.Map String Term) m)
      => Zipper -> m Zipper
whnfMZ (Zipper (Zip branch term)) = case (branch, unTerm term) of
  (App_R arg:zs, I) ->
    do tell [("I", zs `mkZip` (mkI `mkApp` arg))]
       whnfMZ $ zs `mkZip` arg
  (App_R a:App_R b:zs, K) ->
    do tell [("K", zs `mkZip` (mkK `mkApp` a `mkApp` b))]
       whnfMZ $ zs `mkZip` a
  (App_R a:App_R b:App_R c:zs, S) ->
    do tell [("S", mkZip zs (mkS `mkApp` a `mkApp` b `mkApp` c))]
       whnfMZ $ mkZip zs $ a `mkApp` c `mkApp` (b `mkApp` c)
  (App_R a:App_R b:App_R c:zs, B) ->
    do tell [("B", mkZip zs (mkB `mkApp` a `mkApp` b `mkApp` c))]
       whnfMZ $ mkZip zs $ a `mkApp` (b `mkApp` c)
  (App_R a:App_R b:App_R c:zs, C) ->
    do tell [("C", mkZip zs (mkC `mkApp` a `mkApp` b `mkApp` c))]
       whnfMZ $ mkZip zs $ a `mkApp` c `mkApp` b
  (zs, Var name) ->
    do t <- asks $ Map.lookup name
       case t of
         Nothing -> return $ zs `mkZip` mkVar name
         Just t ->
           do tell [("подстановка", zs `mkZip` mkVar name)]
              whnfMZ $ mkZip zs t
  (App_R arg:zs, Lam b i) ->
    do tell [("β", mkZip zs (mkLam b i `mkApp` arg))]
       whnfMZ $ mkZip zs $ subst b arg i
  (zs, App f arg) -> whnfMZ (mkZip (App_R arg:zs) f)
  (zs, t) -> return $ zs `mkZip` Term t


{-
Normal form is as WHNF except all inner terms (E, E1, ...) are in
normal form... To achieve this, first get WHNF. Then move to the next
inner term.
-}

nfMZ :: (MonadWriter [(String, Zipper)] m, MonadReader (Map.Map String Term) m) => (Zipper -> m Zipper) -> Zipper -> m Zipper
nfMZ postProcess z = downwards =<< whnfMZ z
  where downwards (Zipper (Zip (App_R i:zs) j)) = nfMZ postProcess (mkZip (AppL_ j:zs) i)
        downwards (Zipper (Zip zs (Term (Lam b i)))) = 
          do mb <- asks $ Map.lookup b
             local (Map.delete b) $ nfMZ postProcess (mkZip (LamN_e b mb:zs) i)

        downwards z = upwards z

        upwards z = postProcess z >>= upwards'

        upwards' (Zipper (Zip (AppL_ i:zs) j)) = upwards (mkZip zs $ i `mkApp` j)
        upwards' (Zipper (Zip (LamN_e b mb:zs) i)) = local (maybe id (Map.insert b) mb) $ upwards (mkZip zs $ mkLam b i)
        upwards' z@(Zipper (Zip [] _)) = return z

        upwards' z = downwards z

-- And simple version:

whnfM :: (MonadWriter [(String, Zipper)] m, MonadReader (Map.Map String Term) m) => Term -> m Term
whnfM = liftZipper whnfMZ

nfM :: (MonadWriter [(String, Zipper)] m, MonadReader (Map.Map String Term) m) => (Zipper -> m Zipper) -> Term -> m Term
nfM = liftZipper . nfMZ

nf pp env t = runWriter $ runReaderT (nfM pp t) env

whnf :: Map.Map String Term -> Term -> (Term, [(String, Zipper)])
whnf env t = runWriter $ runReaderT (whnfM t) env

eliminateEta :: (MonadWriter [(String, Zipper)] m, MonadReader (Map.Map String Term) m) => (Zipper -> m Zipper)
eliminateEta z@(Zipper (Zip zs (Term (Lam x (Term (App a (Term (Var y)))))))) | x == y && not (y `Set.member` freeVars a) =
  do tell [("η", z)]
     return $ mkZip zs a
eliminateEta z = return z

backSubstitution :: (MonadWriter [(String, Zipper)] m, MonadReader (Map.Map String Term) m) => (Zipper -> m Zipper)
backSubstitution z@(Zipper (Zip zs t)) =
  do env <- ask
     case find ((Alpha t ==) . Alpha . snd) (Map.toList env) of
       (Just (name, _)) ->
         do tell [("обратная подстановка", z)]
            return $ mkZip zs (mkVar name)
       _ -> return z

data ToBasisData = ToBasisData
  { tbdUseEta :: Bool
  , tbdUseK :: Bool
  , tbdUseB :: Bool
  , tbdUseC :: Bool
  }
       
toBasisMZ :: (MonadWriter [(String, Zipper)] m, MonadReader ToBasisData m)
      => Zipper -> m Zipper
toBasisMZ z@(Zipper (Zip branch term)) = -- traceShow term $
  do useEta <- asks tbdUseEta
     useK <- asks tbdUseK
     useB <- asks tbdUseB
     useC <- asks tbdUseC
     case unTerm term of
       (Lam x t) ->
         case unTerm t of
           _ | not (isFree x t) && useK ->
             do tell [("K", z)]
                toBasisMZ $ branch `mkZip` (mkK `mkApp` t)
           (Var y) | x == y ->
             do tell [("I", z)]
                return $ branch `mkZip` mkI
           (App t1 (Term (Var y))) | useEta && x == y && not (isFree x t1) ->
             do tell [("η", z)]
                toBasisMZ $ branch `mkZip` t1
           (App t1 t2) | isFree x t1 && not (isFree x t2) && useC ->
              do tell [("C", z)]
                 toBasisMZ $ branch `mkZip` ((mkC `mkApp` mkLam x t1) `mkApp` t2)
           (App t1 t2) | not (isFree x t1) && isFree x t2 && useB ->
              do tell [("B", z)]
                 toBasisMZ $ branch `mkZip` ((mkB `mkApp` t1) `mkApp` mkLam x t2)
           (App t1 t2) ->
             do tell [("S", z)]
                toBasisMZ $ branch `mkZip` ((mkS `mkApp` mkLam x t1) `mkApp` mkLam x t2)
           (Lam _ _) ->
             do (Zipper (Zip _ t')) <- toBasisMZ $ (LamN_e x Nothing : branch) `mkZip` t
                toBasisMZ $ branch `mkZip` mkLam x t'
           _ -> return z
       (App t1 t2) ->
         do (Zipper (Zip _ t1')) <- toBasisMZ $ mkZip (App_R t2 : branch) t1
            (Zipper (Zip _ t2')) <- toBasisMZ $ mkZip (AppL_ t1' : branch) t2
            return $ branch `mkZip` (t1' `mkApp` t2')
       _ -> return z

toBasisM :: (MonadWriter [(String, Zipper)] m, MonadReader ToBasisData m) => Term -> m Term
toBasisM = liftZipper toBasisMZ

toBasis args t = runWriter $ runReaderT (toBasisM t) args
