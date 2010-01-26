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

-- Parser for lambda-terms

module Lambda.Parser (parser, nameParser) where

import Control.Monad
import Text.ParserCombinators.Parsec as Parsec
import Lambda.Common (Term, mkApp, mkVar, mkLam, mkI, mkK, mkB, mkC, mkS)

-- Main parser

parser :: CharParser () Term
parser = appParser

-- Application is just a secuence of terms

appParser :: CharParser () Term
appParser = 
  do apps <- many1 $ try opParser
     return $ foldl1 mkApp apps

{-
Custom term, except top-level application
Either abstraction or variable
Or maybe some application in parentheses
-}

opParser :: CharParser () Term
opParser =
  do skipMany space
     abstrParser
       <|> between (char '(') (skipMany space >> char ')' <?> "закрывающая скобка") appParser
       <|> varParser

{-
Abstraction starts with lambda "\\"
and contains a sequence of variables
that ends with "." and then some term
-}

abstrParser :: CharParser () Term
abstrParser = (<?> "λ-абстракция") $
  do char '\\' <|> (char 'λ' >> space)
     skipMany space
     abstrNextVarParser
  where abstrVarParser =
          do skipMany space
             abstrDotParser <|> abstrNextVarParser
        abstrNextVarParser =
          do name <- nameParser
             body <- abstrVarParser
             return $ name `mkLam` body
        abstrDotParser = (char '.' >> appParser) <?> "точка"

{-
Variable is just a sequence of characters
-}

varParser :: CharParser () Term
varParser =
  do name <- try nameParser <|> msum (map string ["I", "K", "B", "C", "S"])
     return $ case name of
       "I" -> mkI
       "K" -> mkK
       "B" -> mkB
       "C" -> mkC
       "S" -> mkS
       name -> mkVar name

nameParser = (<?> "имя переменной") $
  do firstLetter <- letter
     lettersOrDigits <- many alphaNum
     primes <- many (char '\'')
     let name = firstLetter : (lettersOrDigits ++ primes)
     when (name `elem` ["I", "K", "B", "C", "S"]) $
       fail "I, K, B, C, S --- зарезервированные идентификаторы"
     return name
