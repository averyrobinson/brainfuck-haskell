import Control.Monad (foldM)

import Data.Word (Word8)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main = do
    source <- getSource
    let tokens = interpret source
        isValid = checkSyntax tokens
    if isValid
        then run tokens emptyTape >> return ()
        else putStrLn "Syntax error."

getSource :: IO String
getSource = do
    args <- getArgs
    if length args == 0
        then error $ "No file path specified."
        else return ()
    let path = head args
    exists <- doesFileExist path
    if exists
        then readFile path
        else error $ "The file \"" ++ path ++ "\" does not exist."


data BFToken = MoveR
             | MoveL
             | Plus
             | Minus
             | PrintChar
             | ReadChar
             | BracketL
             | BracketR
             deriving (Eq, Show)

data Tape = Tape [Word8] Word8 [Word8]
          deriving (Show)

emptyTape :: Tape
emptyTape = Tape (repeat 0) 0 (repeat 0)

interpret :: String -> [BFToken]
interpret = foldr step []
  where
    step '>' = (MoveR :)
    step '<' = (MoveL :)
    step '+' = (Plus :)
    step '-' = (Minus :)
    step '.' = (PrintChar :)
    step ',' = (ReadChar :)
    step '[' = (BracketL :)
    step ']' = (BracketR :)
    step _   = id

checkSyntax :: [BFToken] -> Bool
checkSyntax tokens = foldM count 0 tokens == Just 0
  where
    count acc BracketL = Just $ acc + 1
    count acc BracketR = if acc == 0
                             then Nothing
                             else Just $ acc - 1
    count acc _        = Just acc

run :: [BFToken] -> Tape -> IO Tape
run (MoveR : ts) tape = return (moveR tape) >>= run ts
run (MoveL : ts) tape = return (moveL tape) >>= run ts
run (Plus  : ts) tape = return (plus tape)  >>= run ts
run (Minus : ts) tape = return (minus tape) >>= run ts
run (PrintChar : ts) tape = printChar tape  >>  run ts tape
run (ReadChar : ts)  tape = readChar  tape  >>= run ts
run (BracketL : ts) tape =
    let takeBracket 1 (BracketR : ts) = []
        takeBracket n (BracketL : ts) = BracketL : takeBracket (n + 1) ts
        takeBracket n (BracketR : ts) = BracketR : takeBracket (n - 1) ts
        takeBracket n (t : ts)        = t        : takeBracket  n      ts
        bracket = takeBracket 1 ts
        rest = drop (length bracket + 1) ts
        runBracket tape@(Tape _ m _)
            | m /= 0    = run bracket tape >>= runBracket
            | otherwise = return tape
    in runBracket tape >>= run rest
run [] tape = return tape

moveR :: Tape -> Tape
moveR (Tape ls m (r:rs)) = Tape (m:ls) r rs

moveL :: Tape -> Tape
moveL (Tape (l:ls) m rs) = Tape ls l (m:rs)

plus :: Tape -> Tape
plus (Tape ls m rs) = Tape ls (m + 1) rs

minus :: Tape -> Tape
minus (Tape ls m rs) = Tape ls (m - 1) rs

printChar :: Tape -> IO ()
printChar (Tape _ m _) = putChar . toEnum . fromEnum $ m

readChar :: Tape -> IO Tape
readChar (Tape ls _ rs) = do m <- fmap (toEnum . fromEnum) getChar
                             return (Tape ls m rs)
