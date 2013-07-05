module Interpreter where

import Data.Word
import Data.ByteString.Internal
import Control.Monad

import Parser


data Tape = Tape
	{ tapeToLeft  :: [Word8]
	, currentCell :: Word8
	, tapeToRight :: [Word8]
	}

emptyTape :: Tape
emptyTape = let z = 0:z in Tape z 0 z

exec :: Tape -> Command -> IO (Tape)
exec (Tape (x:xs) y zs) MovePtrLeft = return $ Tape xs x (y:zs)
exec (Tape xs y (z:zs)) MovePtrRight = return $ Tape (y:xs) z zs
exec (Tape xs y zs) Increment = return $ Tape xs (y+1) zs
exec (Tape xs y zs) Decrement = return $ Tape xs (y-1) zs
exec tape@(Tape _ y _) Read = putChar (w2c y) >> return tape
exec (Tape xs y zs) Write = do
	ch <- getChar
	return $ Tape xs (c2w ch) zs
exec tape@(Tape xs y zs) loop@(Loop cmds)
	| y == 0	= return tape
	| otherwise = do
		l <- (foldM exec tape cmds)
		exec l loop

runBrainfuckCode :: [Command] -> IO ()
runBrainfuckCode code = foldM_ exec emptyTape code

runBrainfuckSource source = do
	case parseBrainfuck source of
		Left error -> putStrLn $ "Parse error at" ++ (show error)
		Right code -> runBrainfuckCode code

