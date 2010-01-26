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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
This module implements a history data structure to store history, like browser
history or application UnDo list
-}
module Data.History
  ( -- * History data structure
    History (..)
    -- * Reference implementation
    
    -- | 'add', 'back', 'forward', 'get', 'current' are O(1)
    -- 'goTo' is O(n), where n is 'History' size
  , ListHistory
  , ListHP
  ) where

import Control.Monad

-- | 'History' class defines history data structure
class History hist elem point | hist -> elem, hist -> point
    where empty :: hist -- ^ 'empty' is an empty history
  
          -- | 'back' returns a history with current position moved one step backward
          back :: hist -> Maybe hist
          
          -- | 'forward' returns a history with current position moved one step forward
          forward :: hist -> Maybe hist
          
          -- | 'isStart' tells weather history is at it's start
          isStart :: hist -> Bool
          
          -- | 'isEnd' tells weather history is at it's start
          isEnd :: hist -> Bool
          
          -- | 'add' returns a history where old current element moved to past,
          -- current element is new element and future is empty
          add
            :: elem -- ^ New current element
            -> hist -- ^ History to change future of
            -> hist -- ^ New history with alternative future
          
          -- | 'get' returns current element
          get :: hist -> Maybe elem
          
          -- | 'current' returns current position in the history
          current :: hist -> point
          
          -- | 'goTo' rolles history to position saved by 'current' function
          goTo :: point -> hist -> Maybe hist

-- | 'ListHistory' is a reference implementation of 'History' class
-- based on Zipper data structure
data ListHistory a = LHEmpty | LHist { lhBefore :: [a], lhCurrent :: a, lhAfter :: [a] }

-- | 'ListHP' is a position in 'ListHistory'
data ListHP a = LHP Int

lhStart LHEmpty = LHEmpty
lhStart (LHist bs c as) = LHist [] h (hs ++ [c] ++ as)
  where (h:hs) = reverse bs

instance History (ListHistory a) a (ListHP a)
  where empty = LHEmpty
        back (LHist (b:bs) c as) = Just $ LHist bs b (c:as)
        back _ = Nothing
        isStart LHEmpty = True
        isStart (LHist [] _ _) = True
        isStart _ = False
        forward (LHist bs c (a:as)) = Just $ LHist (c:bs) a as
        forward _ = Nothing
        isEnd LHEmpty = True
        isEnd (LHist _ _ []) = True
        isEnd _ = False
        add x (LHist bs c _) = LHist (c:bs) x []
        add x LHEmpty = LHist [] x []
        get (LHist _ c _) = Just $ c
        get _ = Nothing
        current LHEmpty = LHP 0
        current (LHist bs _ _) = LHP (length bs)
        goTo (LHP n) = foldr1 (<=<) (replicate n forward) . lhStart

