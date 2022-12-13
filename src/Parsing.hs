module Parsing ( integer , Parser ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void ( Void )


type Parser = Parsec Void String

integer :: Parser Int
integer = read <$> some numberChar
