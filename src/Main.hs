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
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (catch, readFile)
import System.IO.UTF8
import qualified Data.Map as Map
import Control.Arrow
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Exception
import Data.IORef
import Data.Time.Clock
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec as Parsec
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.ModelView as GtkMV
import Graphics.UI.Gtk.Glade
import Lambda
import UI
import Paths_LambdaInterpreter

data StepRow = StepRow { srNumber :: Integer, srTerm :: MarkedTerm, srLabel :: String}

data InputState = NotChecked String | Evaled [StepRow] | Error

main =
  do initGUI
     gladeFileName <- getDataFileName "interpreter.glade"
     Just xml <- xmlNew gladeFileName
     window   <- xmlGetWidget xml castToWindow "window-main"
     onDestroy window mainQuit
     (examples, env) <- loadData
     getEnv <- setupEnvironmentUI env xml window
     setupEvaluatorUI examples xml window getEnv
     widgetShowAll window
     mainGUI

setupEnvironmentUI env xml window =
  do envIORef <- newIORef (Map.empty)
     buttonDeleteSelection <- xmlGetWidget xml castToButton "button-env-delete-selection"
     buttonDeleteAll <- xmlGetWidget xml castToButton "button-env-delete-all"
     entryName <- xmlGetWidget xml castToEntry "entry-env-name"
     entryValue <- xmlGetWidget xml castToEntry "entry-env-value"
     buttonAdd <- xmlGetWidget xml castToButton "button-env-add"
     (envList, envView) <- setupEnvTreeView xml
     
     let updateEnv env =
           do writeIORef envIORef env
              GtkMV.listStoreClear envList
              mapM_ (GtkMV.listStoreAppend envList) $ Map.toAscList env
     updateEnv env
     onClicked buttonAdd $
       do nameText <- get entryName entryText
          let parseRes = Parsec.parse (skipMany space >> Lambda.nameParser >>= \t -> skipMany space >> eof >> return t) "" nameText
          case parseRes of
            (Left err) ->
              do msgBox (Just window) [] MessageWarning ButtonsOk $ "Wrong name: " ++ show err
                 return ()
            (Right name) ->
               do valueText <- get entryValue entryText
                  let parseRes = Parsec.parse (skipMany space >> Lambda.parser >>= \t -> skipMany space >> eof >> return t) "" valueText
                  case parseRes of
                    (Left err) -> 
                      do msgBox (Just window) [] MessageWarning ButtonsOk $ "Wrong value: " ++ show err
                         return ()
                    (Right value) ->
                      do env <- readIORef envIORef
                         if Map.member name env
                           then msgBox (Just window) [] MessageWarning ButtonsOk "Value with given name already exists" >> return ()
                           else do env <- readIORef envIORef
                                   updateEnv $ Map.insert name value env
                                   
     onClicked buttonDeleteAll $ updateEnv Map.empty
     onClicked buttonDeleteSelection $
       do selection <- GtkMV.treeViewGetSelection envView
          rows <- GtkMV.treeSelectionGetSelectedRows selection
          nodes <- mapM (listStoreGetValue envList . head) rows
          env <- readIORef envIORef
          updateEnv $ Map.filterWithKey (\n _ -> not $ n `elem` map fst nodes) env
          
     return (readIORef envIORef)

setupEnvTreeView xml =
  do  view <- xmlGetWidget xml castToTreeView "treeview-env"
      model <- GtkMV.listStoreNew []
      GtkMV.treeViewSetModel view model

      GtkMV.treeViewSetHeadersVisible view True
      
      selection <- GtkMV.treeViewGetSelection view
      GtkMV.treeSelectionSetMode selection SelectionMultiple

      -- add a couple columns
      col1 <- GtkMV.treeViewColumnNew
      col2 <- GtkMV.treeViewColumnNew

      GtkMV.treeViewColumnSetTitle col1 "Name"
      GtkMV.treeViewColumnSetTitle col2 "Value"

      renderer1 <- GtkMV.cellRendererTextNew
      renderer2 <- GtkMV.cellRendererTextNew

      GtkMV.cellLayoutPackStart col1 renderer1 True
      GtkMV.cellLayoutPackStart col2 renderer2 True

      GtkMV.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ GtkMV.cellText := fst row ]
      GtkMV.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ GtkMV.cellText := (show . Pango . snd) row ]

      GtkMV.treeViewAppendColumn view col1
      GtkMV.treeViewAppendColumn view col2

      return (model, view)

     
setupEvaluatorUI examples xml window getEnv =
  do comboBoxEntryInput <- xmlGetWidget xml castToComboBoxEntry "comboboxentry-examples"
     resultIORef <- newIORef (NotChecked "")
     stepsList <- setupEvalTreeView xml
     buttonResult <- xmlGetWidget xml castToButton "button-result"
     button1Step <- xmlGetWidget xml castToButton "button-1step"
     buttonNSteps <- xmlGetWidget xml castToButton "button-nsteps"
     buttonAllSteps <- xmlGetWidget xml castToButton "button-allsteps"
     buttonStepsClear <- xmlGetWidget xml castToButton "button-steps-clear"
     entryNSteps <- xmlGetWidget xml castToEntry "entry-nsteps"
     checkbuttonEtaReduction <- xmlGetWidget xml castToCheckButton "checkbutton-eta-reduction"
     checkbuttonBackSubstitution <- xmlGetWidget xml castToCheckButton "checkbutton-back-substitution"
     checkbuttonDisableK <- xmlGetWidget xml castToCheckButton "checkbutton-disablek"
     statusbar <- xmlGetWidget xml castToStatusbar "statusbar"
     timeElapsedIORef <- newIORef 0
     setupExamples comboBoxEntryInput examples
     radioButtonNf <- xmlGetWidget xml castToRadioButton "radiobutton-nf"
     radioButtonWhnf <- xmlGetWidget xml castToRadioButton "radiobutton-whnf"
     radioButtonToIs <- xmlGetWidget xml castToRadioButton "radiobutton-tois"
     radioButtonToIbcs <- xmlGetWidget xml castToRadioButton "radiobutton-toibcs"
      
     -- Model:
     let updateButtonStates =
           do result <- readIORef resultIORef
              case result of
                (NotChecked _) ->
                  enable [(buttonResult, True), (button1Step, True), (buttonNSteps, True), (buttonAllSteps, True), (buttonStepsClear, True)]
                (Evaled (_:_)) ->
                  enable [(buttonResult, True), (button1Step, True), (buttonNSteps, True), (buttonAllSteps, True), (buttonStepsClear, True)]
                (Evaled []) ->
                  enable [(buttonResult, True), (button1Step, False), (buttonNSteps, False), (buttonAllSteps, False), (buttonStepsClear, True)]
                Error ->
                  enable [(buttonResult, True), (button1Step, False), (buttonNSteps, False), (buttonAllSteps, False), (buttonStepsClear, True)]
         countTime action =
           do start <- getCurrentTime
              action
              end <- getCurrentTime
              time <- readIORef timeElapsedIORef
              let time' = time + diffUTCTime end start
                  msg = "Duration of evaluation: " ++ show time'
              writeIORef timeElapsedIORef time'
              contextId <- statusbarGetContextId statusbar msg
              statusbarPush statusbar contextId msg
              return ()
         updateNfFlagsSensitivity =
           do isNf <- get radioButtonNf toggleButtonActive
              _isWhnf <- get radioButtonWhnf toggleButtonActive
              isToIs <- get radioButtonToIs toggleButtonActive
              isToIbcs <- get radioButtonToIbcs toggleButtonActive
              set checkbuttonEtaReduction [widgetSensitive := isNf || isToIs || isToIbcs]
              set checkbuttonBackSubstitution [widgetSensitive := isNf]
              set checkbuttonDisableK [widgetSensitive := isToIs || isToIbcs]
         getPostProcessor :: (MonadWriter [(String, Zipper)] m, MonadReader (Map.Map String Term) m) => IO (Zipper -> m Zipper)
         getPostProcessor =
           do eta <- toggleButtonGetActive checkbuttonEtaReduction
              bsubst <- toggleButtonGetActive checkbuttonBackSubstitution
              return $ \z ->
                do z' <- (if eta then eliminateEta else return) z
                   z'' <- (if bsubst then backSubstitution else return) z'
                   return z''
         getProcessor =
           do isNf <- get radioButtonNf toggleButtonActive
              isWhnf <- get radioButtonWhnf toggleButtonActive
              isToIs <- get radioButtonToIs toggleButtonActive
              isToIbcs <- get radioButtonToIbcs toggleButtonActive
              snd . fromJust . find fst $
                  [(isNf, 
                    do pp <- getPostProcessor
                       return $ nf pp)
                  ,(isWhnf, return whnf)
                  ,(isToIs,
                    do eta <- toggleButtonGetActive checkbuttonEtaReduction
                       notUseK <- toggleButtonGetActive checkbuttonDisableK
                       return $ const $
                         toBasis $ ToBasisData
                           { tbdUseEta = eta
                           , tbdUseK = not notUseK
                           , tbdUseB = False
                           , tbdUseC = False } )
                  ,(isToIbcs,
                    do eta <- toggleButtonGetActive checkbuttonEtaReduction
                       notUseK <- toggleButtonGetActive checkbuttonDisableK
                       return $ const $
                         toBasis $ ToBasisData
                           { tbdUseEta = eta
                           , tbdUseK = not notUseK
                           , tbdUseB = True
                           , tbdUseC = True } )
                  ,(True,
                    do let msg = "Panic: Radiobutton's value wich defined what\
                                  \ to evaluate is insane!" 
                       msgBox (Just window) [] MessageWarning ButtonsOk $ msg
                       error msg)
                  ]
         cleanSteps =
           do (Just text) <- comboBoxEntryGetActiveText comboBoxEntryInput
              writeIORef resultIORef (NotChecked text)
              writeIORef timeElapsedIORef 0
              -- updateButtonStates
         getNSteps f =
           do text <- get entryNSteps entryText
              (readIO text >>= \(i :: Int) -> f i) `catch` \(e::SomeException) ->
                do msgBox (Just window) [] MessageWarning ButtonsOk $ "Number of steps is wrong: " ++ show text ++ ": " ++ show e
                   return ()
         getSteps f =
           do steps <- readIORef resultIORef
              case steps of
                Error -> error "button pressed when must be disabled"
                (Evaled ss) -> countTime $ f ss
                (NotChecked text) ->
                  do let exprRes = Parsec.parse (Lambda.parser >>= \t -> skipMany space >> eof >> return t) "" text
                     case exprRes of
                       (Left err) ->
                         do msgBox (Just window) [] MessageWarning ButtonsOk $ "Wrong text: " ++ show err
                            writeIORef resultIORef Error
                            return ()
                       (Right term) ->
                         do env <- getEnv
                            processor <- getProcessor
                            let (res, ss) = processor env term
                                steps = zipWith (\n (l, t) -> StepRow n t l) [1..] $
                                          map (second (unwind . zipperToMarked)) ss ++ [("", termToUnmarked res)]
                            writeIORef resultIORef (Evaled steps)
                            GtkMV.listStoreClear stepsList
                            contextId <- statusbarGetContextId statusbar ""
                            statusbarPush statusbar contextId ""
                            countTime $ f steps
              updateButtonStates
              
     -- Initialization:
     updateButtonStates
     
     -- Controller:
     onToggled checkbuttonDisableK $
       do notUseK <- get checkbuttonDisableK toggleButtonActive
          if notUseK
            then do set radioButtonToIs [buttonLabel := "Convert to (I, S)"]
                    set radioButtonToIbcs [buttonLabel := "Convert to (I, B, C, S)"]
            else do set radioButtonToIs [buttonLabel := "Convert to (I, K, S)"]
                    set radioButtonToIbcs [buttonLabel := "Convert to (I, K, B, C, S)"]
     do entry <- binGetChild comboBoxEntryInput
        case entry of
         (Just entry) -> onEditableChanged (castToEntry entry) cleanSteps >> return ()
         _ -> return ()              
     onToggled radioButtonNf updateNfFlagsSensitivity
     onToggled radioButtonWhnf updateNfFlagsSensitivity
     onToggled radioButtonToIs updateNfFlagsSensitivity
     onToggled radioButtonToIbcs updateNfFlagsSensitivity
     onClicked buttonResult $ (cleanSteps >>) $ getSteps $ \ss ->
       case ss of
         [] -> cleanSteps
         ss ->
           do entry <- binGetChild comboBoxEntryInput
              case entry of
               (Just entry) ->
                 do cleanSteps
                    entrySetText (castToEntry entry) (show . srTerm . last $ ss)
                    GtkMV.listStoreClear stepsList
               _ -> return ()
     
     onClicked button1Step $ getSteps $ \ss ->
       do GtkMV.listStoreAppend stepsList (head ss)
          writeIORef resultIORef (Evaled $ tail ss)
     
     onClicked buttonNSteps $ getSteps $ \ss -> getNSteps $ \n ->
       do let (ls, rest) = splitAt n ss
          mapM_ (GtkMV.listStoreAppend stepsList) ls
          writeIORef resultIORef (Evaled rest)
     
     onClicked buttonAllSteps $ getSteps $ \ss ->
       do mapM_ (GtkMV.listStoreAppend stepsList) ss
          writeIORef resultIORef (Evaled [])
     
     onClicked buttonStepsClear $ cleanSteps >> updateButtonStates >> GtkMV.listStoreClear stepsList

setupEvalTreeView xml =
  do  view <- xmlGetWidget xml castToTreeView "treeview-steps"
      model <- GtkMV.listStoreNew []
      GtkMV.treeViewSetModel view model

      GtkMV.treeViewSetHeadersVisible view True

      -- add a couple columns
      col1 <- GtkMV.treeViewColumnNew
      col2 <- GtkMV.treeViewColumnNew
      col3 <- GtkMV.treeViewColumnNew
      
      treeViewColumnSetSizing col1 TreeViewColumnAutosize
      treeViewColumnSetSizing col2 TreeViewColumnAutosize
      treeViewColumnSetSizing col3 TreeViewColumnAutosize

      GtkMV.treeViewColumnSetTitle col1 "Axiom scheme"
      GtkMV.treeViewColumnSetTitle col2 "Term"
      GtkMV.treeViewColumnSetTitle col3 "Step's number"

      renderer1 <- GtkMV.cellRendererTextNew
      renderer2 <- GtkMV.cellRendererTextNew
      renderer3 <- GtkMV.cellRendererTextNew

      GtkMV.cellLayoutPackStart col1 renderer1 True
      GtkMV.cellLayoutPackStart col2 renderer2 True
      GtkMV.cellLayoutPackStart col3 renderer3 True

      GtkMV.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ GtkMV.cellText := srLabel row ]
      GtkMV.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ GtkMV.cellTextMarkup := Just (show . Pango . srTerm $ row) ]
      GtkMV.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ GtkMV.cellText := (show . srNumber) row ]

      GtkMV.treeViewAppendColumn view col3
      GtkMV.treeViewAppendColumn view col2
      GtkMV.treeViewAppendColumn view col1

      return model

setupExamples view examples = 
  do _model <- comboBoxEntrySetModelText view
     mapM_ (comboBoxAppendText view . show . Pango) examples

loadData :: IO ([Term], Map.Map String Term)     
loadData = 
  do examples <- handle (\(e :: SomeException) -> msgBox Nothing [] MessageWarning ButtonsOk ("Error reading examples file: " ++ show e) >> return []) $
       do examplesFileName <- getDataFileName "examples.txt"
          examplesLines <- fmap lines $ readFile examplesFileName
          let parsings :: [Term]
              parsings = concatMap (fromEither . parse) examplesLines
              parse :: String -> Either ParseError Term
              parse = Parsec.parse (Lambda.parser >>= \t -> skipMany space >> eof >> return t) ""
              fromEither :: Either ParseError Term -> [Term]
              fromEither = either (const []) (\t -> [t])
          return parsings
     env <- handle (\(e :: SomeException) -> msgBox Nothing [] MessageWarning ButtonsOk ("Error reading evironment file: " ++ show e) >> return Map.empty) $
       do environmentFileName <- getDataFileName "environment.txt"
          environmentLines <- fmap lines $ readFile environmentFileName
          let parsings :: Map.Map String Term
              parsings = Map.fromList $ concatMap (fromEither . parse) environmentLines
              parse :: String -> Either ParseError (String, Term)
              parse = Parsec.parse parser ""
              parser =
                do skipMany space
                   name <- Lambda.nameParser
                   skipMany space
                   value <- Lambda.parser
                   skipMany space
                   eof
                   return (name, value)
              fromEither :: Either ParseError (String, Term) -> [(String, Term)]
              fromEither = either (const []) (\t -> [t])
          return parsings
     return (examples, env)--(examples, env)
