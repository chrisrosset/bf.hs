module Main where

import Data.Char
import Data.List
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Interpreter

main = do
	path <- getArgs >>= parse
	code <- readFile path
	runBrainfuckSource code

-- Command line argument parsing
-- http://www.haskell.org/haskellwiki/Tutorials/Programming_Haskell/Argument_handling#GetOpt
data Flag
	= Help
	deriving (Eq,Ord,Enum,Show,Bounded)

flags =
	[ Option [] ["help"] (NoArg Help) "Print this help message"
	]

parse argv = case getOpt Permute flags argv of
	(args, fs, []) -> do
		if Help `elem` args || null fs || length fs > 1
			then do
				hPutStrLn stderr (usageInfo header flags)
				exitWith ExitSuccess
			else return $ head fs
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo header flags)
		exitWith (ExitFailure 1)
	where
		header = "Usage: brainfuck [OPTION] FILE"
