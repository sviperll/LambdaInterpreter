module Widgets
  (
  ) where

class GladeLoadable widget settings | widget -> settings
  where load :: settings -> name -> GladeXML -> IO widget

class Creatable widget settings | widget -> settings
  where create :: settings -> IO widget

data HistoryWidget a = ...

data HistoryWidgetSettings a =
  HistoryWidgetSettings
    { toString :: a -> String
    }

loadHistoryWidget (HistoryWidgetSettings { toString = toS }) =
  do


containerClear
  :: (ContainerClass self)	
  => self	
  -> IO ()
containerClear = mapM_ (containerRemove self) <=< containerGetChildren

addToHistoryWidget :: a -> HistoryWidget a -> IO ()  
addToHistoryWidget x historyWidget =
  do hist <- readIORef histRef
     let hist' = addToHistory x hist'
     writeIORef histRef hist'
     containerClear menu
     fillMenu hist'
     setButtons hist'
  where fillMenu hist = case listFromHistory hist of
          [] -> return ()
          (x:xs) ->
            do firstItem <- radioMenuItemNewWithLabel (toStr x)
               menuShellAppend menu firstItem
               forM_ xs $ \x ->
                 do item <- radioMenuItemNewWithLabelFromWidget firstItem (toStr x)
                    menuShellAppend menu item
        setButtons hist =
          do set backButton [ widgetSensitive := not (isStartInHistory hist) ]
             set fwdButton [ widgetSensitive := not (isEndInHistory hist) ]
             set jumpToButton [ widgetSensitive := not (isEmptyHistory hist) ]
        toStr = toStringOfHistoryWidget historyWidget
        histRef = histRefOfHistoryWidget historyWidget
        menu = menuOfHistoryWidget historyWidget
        backButton = backButtonOfHistoryWidget historyWidget
        fwdButton = fwdButtonOfHistoryWidget historyWidget
        jumpToButton = jumpToButtonOfHistoryWidget historyWidget

onChangeOfHistoryWidget historyWidget action =
  do writeIORef onChageHandlerRef action
     