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
{- |
  This module implements history widget to store application history
  And to navigate back and forward through it
-}
module Widget.History
  (
  ) where

import Data.History as DH

-- | Data type fore history widget
data HistoryWidget a = HistW
  { hwBtnBack :: Button }

-- | 'loadFromGlade' load history widget from 'GladeXML' file
loadFromGlade
  :: String -- ^ Name of the widget
  -> (a -> String) -- ^ Function to convert data into it's string representation
  -> GladeXML -- ^ Glade file
  -> IO (HistoryWidget a)
loadFromGlade =
  do buttonBack <- xmlGetWidget xml castToButton "button-hist-back"
     buttonForward <- xmlGetWidget xml castToButton "button-hist-forward"
     buttonJumpTo <- xmlGetWidget xml castToButton "button-hist-jumpto"
     menuHist <- menuNew
     hist <- newIORef $ (empty :: ListHistory a)
     onClicked buttonJumpTo $ menuPopup menuHist Nothing
     return HistoryWidget
       { hwBtnBack = buttonBack
       , hwBtnForward = buttonForward
       , hwBtnJumpTo = buttonJumpTo
       , hwMenu = menuHist
       , hwData = hist
       }

add :: a -> HistoryWidget a -> IO ()
add x (HistW {hwData = hist})=
  do modifyIORef hist (DH.add x)
     