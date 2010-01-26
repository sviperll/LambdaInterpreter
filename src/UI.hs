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

module UI (msgBox, Pango (Pango), enable) where

import Graphics.UI.Gtk
import Lambda

msgBox :: Maybe Window -> [DialogFlags] -> MessageType -> ButtonsType -> String -> IO ResponseId
msgBox wnd flags typ buttons msg =
  do dialog <- messageDialogNew wnd flags typ buttons msg
     widgetShow dialog
     response <- dialogRun dialog
     widgetHide dialog
     return response

enable xs = mapM_ (\(w, s) -> set w [widgetSensitive := s]) xs

newtype Pango a = Pango { unPango :: a }

instance Show (Pango Zipper)
  where show = show . unwind . zipperToMarked . unPango

instance Show (Pango Term)
  where show = show . Pango . termToUnmarked . unPango

instance Show (Pango MarkedTerm)
  where show (Pango (UnmarkedTerm t)) = case t of
          I -> "I"
          K -> "K"
          B -> "B"
          C -> "C"
          S -> "S"
          (Var b) -> b
          (Lam b i) ->
            let showl (UnmarkedTerm (Lam b i)) = " " ++ b ++ showl i
                showl i = ". " ++ show (Pango i)
            in "λ " ++ b ++ showl i                
          (App i j) ->
            let showLeft i@(UnmarkedTerm (Lam _ _)) = "(" ++ show (Pango i) ++ ")"
                showLeft i = show (Pango i)
                showRight i@(UnmarkedTerm (Lam _ _)) = "(" ++ show (Pango i) ++ ")"
                showRight i@(UnmarkedTerm (App _ _)) = "(" ++ show (Pango i) ++ ")"
                showRight i = show (Pango i)
            in showLeft i ++ " " ++ showRight j
        show (Pango (MarkedTerm i)) = "<span font-weight=\"bold\"> ( " ++ (show . Pango . UnmarkedTerm $ i) ++ " ) </span>"
