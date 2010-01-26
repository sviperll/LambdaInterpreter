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
{-# LANGUAGE FlexibleInstances #-}
{- |
This module defines lamda terms and their basic properties

Conversion relation is defined on terms, for ex.
(\x.x) a = a

``='' is a symbol of conversion relation
``(\x.x) a'' and ``a'' are terms
-}
module Lambda.Common
  (Term (..)
  ,TermF (..)
  ,MarkedTerm (..)
  ,subst
  ,isFree
  ,freeVars
  ,mkI
  ,mkK
  ,mkB
  ,mkC
  ,mkS
  ,mkVar
  ,mkLam
  ,mkApp
  ,termToMarked
  ,termToUnmarked
  ,removeMarks
  ,Alpha (Alpha)) where

import Data.Set as Set (Set, member, singleton, union, delete, empty)

-- | 'TermF' defines types that can represent lambda terms
data TermF subterm
  = S | K | I | B | C
  | Var String -- ^ 'Var' creates a Single variable
  | Lam String subterm -- ^ 'Lam' creates an abstraction which binds variable all over inner term
  | App subterm subterm -- ^ 'App' is an application, right term is a value of
                        -- variable abstracted in left term

-- | 'Term' is a type that can represent only lambda terms
newtype Term = Term {unTerm :: TermF Term}

-- | 'MarkedTerm' is a type that can represent only lambda terms
-- with parts of two kinds: marked and unmarked
data MarkedTerm
  = MarkedTerm (TermF MarkedTerm)
  | UnmarkedTerm (TermF MarkedTerm)

-- | 'mkI' constructs 'I' combnator of 'Term' type
mkI = Term I
-- | 'mkK' constructs 'K' combnator of 'Term' type
mkK = Term K
-- | 'mkB ' constructs 'B' combnator of 'Term' type
mkB = Term B
-- | 'mkC' constructs 'C' combnator of 'Term' type
mkC = Term C
-- | 'mkS' constructs 'S' combnator of 'Term' type
mkS = Term S
-- | 'mkVar' constructs 'Var' of 'Term' type
mkVar = Term . Var
-- | 'mkLam' constructs 'Lam' of 'Term' type
mkLam name body = Term $ Lam name body
-- | 'mkApp' constructs 'App' combnator of 'Term' type
mkApp func arg = Term $ App func arg

instance Functor TermF
  where fmap _ (Var name) = Var name
        fmap _ I = I
        fmap _ K = K
        fmap _ B = B
        fmap _ C = C
        fmap _ S = S
        fmap f (Lam name body) = Lam name (f body)
        fmap f (App func arg) = App (f func) (f arg)

termToUnmarked, termToMarked :: Term -> MarkedTerm
termToUnmarked = UnmarkedTerm . fmap termToUnmarked . unTerm
termToMarked = MarkedTerm . fmap termToUnmarked . unTerm

removeMarks (MarkedTerm t) = Term $ fmap removeMarks t
removeMarks (UnmarkedTerm t) = Term $ fmap removeMarks t

alphaEq :: Term -> Term -> Bool
alphaEq (Term (Var a)) (Term (Var b)) = a == b
alphaEq (Term (App a b)) (Term (App a' b')) =
  a `alphaEq` a' && b `alphaEq` b'
alphaEq (Term (Lam n a)) (Term (Lam m b))
  | n == m = a `alphaEq` b
  | not $ Set.member n fvB = a `alphaEq` subst m (mkVar n) b
  | otherwise = alphaEq a $ subst m (mkVar n) . subst n (mkVar n') $ b
    where fvB = freeVars b
          n' = newBinder (fvB `Set.union` freeVars a) n
          newBinder set = head . dropWhile (`Set.member` set) . tail . iterate (++"'")
alphaEq _ _ = False

-- | Alpha Term are terms with defined equality, following alpha reduction
newtype Alpha a = Alpha a

instance Eq (Alpha Term)
  where (Alpha t) == (Alpha t') = t `alphaEq` t'


{- |
The most important function on terms is substituting term on each occurance of
variable
('subst' @b@ @replacement@ @in@)
will replace each occurance of variable @b@ in term @in@ with @replacement@
-}
subst :: String -> Term -> Term -> Term
subst b (Term (Var b')) i
  | b == b' = i
subst b r i@(Term (Var b'))
  | b == b' = r
  | otherwise = i
subst _ _ i@(Term I) = i
subst _ _ i@(Term K) = i
subst _ _ i@(Term B) = i
subst _ _ i@(Term C) = i
subst _ _ i@(Term S) = i
subst b r (Term (App i j)) = subst b r i `mkApp` subst b r j
subst b r i@(Term (Lam b' j))
  | b == b' = i
  | not $ Set.member b' fvR = mkLam b' $ subst b r j
  | otherwise = mkLam b'' $ subst b r . subst b' (mkVar b'') $ j
    where fvR = freeVars r
          b'' = newBinder (fvR `Set.union` freeVars j) b'
          newBinder set = head . dropWhile (`Set.member` set) . tail . iterate (++"'")

-- | 'freeVars' @t@ defines a set of not binded variables in @t@
freeVars :: Term -> Set.Set String
freeVars (Term I) = Set.empty
freeVars (Term K) = Set.empty
freeVars (Term B) = Set.empty
freeVars (Term C) = Set.empty
freeVars (Term S) = Set.empty
freeVars (Term (Var b)) = Set.singleton b
freeVars (Term (App i j)) = freeVars i `Set.union` freeVars j
freeVars (Term (Lam b i)) = Set.delete b $ freeVars i

-- | 'isFree' @b@ @t@ checkes wheather @b@ is in the set of free variables of term @t@
isFree :: String -> Term -> Bool
isFree b i = Set.member b $ freeVars i
