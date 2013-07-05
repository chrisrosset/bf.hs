module Parser where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

-- https://en.wikipedia.org/wiki/Brainfuck#Commands
data Command
	= MovePtrRight
	| MovePtrLeft
	| Increment
	| Decrement
	| Read
	| Write
	| Loop [Command]
	deriving (Show)

charCmdPairs = [ ( '>', MovePtrRight )
               , ( '<', MovePtrLeft  )
               , ( '+', Increment    )
               , ( '-', Decrement    )
               , ( '.', Read         )
               , ( ',', Write        )
               ]

parseBrainfuck source = parse (many command) "brainfuck code" filtered
	where
		filtered = filter (`elem` "><+-.,[]") source -- remove comments

command = choice (parseLoopCmd : nonLoopCmdParsers)
	where
		nonLoopCmdParsers =  map (uncurry parseNonLoopCmd) charCmdPairs

		parseNonLoopCmd :: Char -> Command -> Parser Command
		parseNonLoopCmd ch cmd = char ch >> return cmd

		parseLoopCmd :: Parser Command
		parseLoopCmd = Loop <$> (char '[' *> many command <* char ']') <?> "loop"
