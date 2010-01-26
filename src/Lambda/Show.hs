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

-- Just some instances for read and show

module Lambda.Show (Braces (Braces)) where

import Lambda.Common
  (Term (..)
  ,MarkedTerm (..)
  ,TermF (..)
  ,termToUnmarked)
import Text.ParserCombinators.Parsec as Parser (parse, many, anyChar)
import Lambda.Parser (parser)

instance Show MarkedTerm
 where show (UnmarkedTerm t) =
         case t of
           I -> "I"
           K -> "K"
           B -> "B"
           C -> "C"
           S -> "S"
           (Var b) -> b
           (Lam b i) ->
             let showl (UnmarkedTerm (Lam b i)) = " " ++ b ++ showl i
                 showl i = ". " ++ show i
             in "\\" ++ b ++ showl i
           (App i j) -> showLeft i ++ " " ++ showRight j
       show (MarkedTerm i) = "  [ " ++ show (UnmarkedTerm i) ++ " ]  "

showLeft i@(UnmarkedTerm (Lam _ _)) = "(" ++ show i ++ ")"
showLeft i = show i

showRight i@(UnmarkedTerm (Lam _ _)) = "(" ++ show i ++ ")"
showRight i@(UnmarkedTerm (App _ _)) = "(" ++ show i ++ ")"
showRight i = show i

instance Show Term
  where show = show . termToUnmarked

newtype Braces a = Braces { unBraces :: a }

showBraces' :: (String -> String) -> Term -> String
showBraces' braceIt (Term t) =
  case t of
    I -> "I"
    K -> "K"
    B -> "B"
    C -> "C"
    S -> "S"
    (Var name) -> name
    (Lam name body) ->
      braceIt $ "\\" ++ name ++ ". " ++ showBraces' bracer body
    (App func arg) ->
      braceIt $ showBraces' bracer func ++ " " ++ showBraces' bracer arg
  where bracer a = "(" ++ a ++ ")" 
  
instance Show (Braces Term)
  where show = showBraces' id . unBraces

instance Read Term
  where readsPrec _n = either (const []) (:[]) . parse p ""
          where p =
                  do t <- parser
                     rest <- many anyChar
                     return (t, rest)
