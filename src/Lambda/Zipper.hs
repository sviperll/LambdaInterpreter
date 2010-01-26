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

{-
Zipper is a "pointer" to any place in term. It allows us to traverse
tree what ever way we want it.
-}

module Lambda.Zipper where

import Lambda.Common
import Lambda.Show ()

data BranchF term = App_R term | AppL_ term | LamN_e String (Maybe term)
data ZipperF term = Zip [BranchF term] term

newtype Zipper = Zipper { unZipper :: ZipperF Term }
newtype MarkedZipper = MarkedZipper (ZipperF MarkedTerm)

mkZip b t = Zipper $ Zip b t

instance Functor ZipperF
  where fmap f (Zip b t) = map (fmap f) b `Zip` f t

instance Functor BranchF
  where fmap f (App_R t) = App_R (f t)
        fmap f (AppL_ t) = AppL_ (f t)
        fmap f (LamN_e n mt) = LamN_e n (fmap f mt)


-- If we have this pointer we can get back to the root of term

zipperToUnmarked :: Zipper -> MarkedZipper
zipperToUnmarked = MarkedZipper . fmap termToUnmarked . unZipper
zipperToMarked (Zipper (Zip b t)) =
  MarkedZipper $ map (fmap termToUnmarked) b `Zip` termToMarked t

unwind :: MarkedZipper -> MarkedTerm
unwind (MarkedZipper (Zip [] i)) = i
unwind (MarkedZipper (Zip (App_R i:zs) j)) = unwind $ MarkedZipper $ Zip zs $ UnmarkedTerm $ App j i
unwind (MarkedZipper (Zip (AppL_ i:zs) j)) = unwind $ MarkedZipper $ Zip zs $ UnmarkedTerm $ App i j
unwind (MarkedZipper (Zip (LamN_e b _t:zs) i)) = unwind $ MarkedZipper $ Zip zs $ UnmarkedTerm $ Lam b i

zipperToTerm :: Zipper -> Term
zipperToTerm = removeMarks . unwind . zipperToUnmarked


-- liftZipper takes zipper transformer and creates term transformer from it.

termToZipper :: Term -> Zipper
termToZipper = Zipper . Zip []


liftZipper :: (Monad m)
           => (Zipper -> m Zipper)
           -> (Term -> m Term)         
liftZipper funcZ t =
  do z <- funcZ . termToZipper $ t
     return $ zipperToTerm z 

instance Show Zipper
  where show = show . unwind . zipperToMarked

