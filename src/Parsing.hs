module Parsing ( integer , Parser , runP ) where

import Text.Megaparsec ( parse, Parsec )
import Text.Megaparsec.Char ( numberChar )
import Text.Megaparsec.Char.Lexer ( signed, decimal )
import Data.Void ( Void )


type Parser = Parsec Void String

integer :: Parser Int
integer = signed (pure ()) decimal

runP :: Parser x -> x -> String -> x
runP p def str = case (parse p "" str) of
     Right x -> x
     Left _  -> def