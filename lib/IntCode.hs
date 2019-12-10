module IntCode
  ( parse
  , run
  , runInput
  , runPropagate
  , Pointer(..)
  , Offset(..)
  )
where

import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import           Data.Vector                              ( (!?)
                                                          , (//)
                                                          , Vector
                                                          )
import qualified Data.Vector                   as Vector

type Program = Vector Int
type Updates = [(Pointer, Int)]

newtype Pointer = Pointer Int
  deriving (Show, Num)

newtype Offset = Offset Int
  deriving (Show, Num)

type Input = Vector Int
type Output = Vector Int

data Mode = Position Pointer | Immediate Int
  deriving (Show)

data Instr a where
  Add ::a -> a -> Pointer -> Instr a
  Mul ::a -> a -> Pointer -> Instr a
  Input ::Pointer -> Instr a
  Output ::a -> Instr a
  JumpIfTrue ::a -> a -> Instr a
  JumpIfFalse ::a -> a -> Instr a
  LessThan ::a -> a -> Pointer -> Instr a
  Equals ::a -> a -> Pointer -> Instr a
  Terminate ::Instr a

deriving instance Functor Instr
deriving instance (Show a) => Show (Instr a)

incPointer :: Pointer -> Offset -> Pointer
incPointer (Pointer p) (Offset o) = Pointer (p + o) 

updateProgram :: Program -> Updates -> Program
updateProgram prog updates = prog // ((\(Pointer p, v) -> (p, v)) <$> updates)

evalArg :: Program -> Mode -> Int
evalArg prog (Position p) = readValue prog p 0
evalArg _ (Immediate i) = i

-- Can't just use Functor as write location arguments must be positions
evalInstr :: Program -> Instr Mode -> Instr Int
evalInstr prog instr = fmap (evalArg prog) instr

-- Step once the values of instructions have been found
step :: Pointer -> Instr Int -> Maybe Int -> (Pointer, Maybe Int, Updates)
step p Terminate _ = (p, Nothing, [])
step p (Add arg1 arg2 arg3) _ = (incPointer p 4, Nothing, [(arg3, arg1 + arg2)])
step p (Mul arg1 arg2 arg3) _ = (incPointer p 4, Nothing, [(arg3, arg1 * arg2)])
step p (Input arg1) (Just input) = (incPointer p 2, Nothing, [(arg1, input)])
step p (Output arg1) _ = (incPointer p 2, Just arg1, [])
step p (JumpIfTrue arg1 arg2) _ =
  (if arg1 /= 0 then Pointer arg2 else incPointer p 3, Nothing, [])
step p (JumpIfFalse arg1 arg2) _ =
  (if arg1 == 0 then Pointer arg2 else incPointer p 3, Nothing, [])
step p (LessThan arg1 arg2 arg3) _ =
  (incPointer p 4, Nothing, [(arg3, if arg1 < arg2 then 1 else 0)])
step p (Equals arg1 arg2 arg3) _ =
  (incPointer p 4, Nothing, [(arg3, if arg1 == arg2 then 1 else 0)])

run :: Program -> Input -> (Output, Program)
run initialProg input =
  let (_, o, finalProg) = go 0 (input, Vector.empty, initialProg)
  in  (o, finalProg)
 where
  go :: Pointer -> (Input, Output, Program) -> (Input, Output, Program)
  go _ (i, o, prog@(Vector.null -> True)     ) = (i, o, prog)
  go p (i, o, prog@(readInstr p -> Terminate)) = (i, o, prog)
  go p (i, o, prog@(evalInstr prog . readInstr p -> instr@(Input _))) =
    let (newP, _, updates) = step p instr (Just (Vector.head i))
    in  go newP (Vector.tail i, o, updateProgram prog updates)
  go p (i, o, prog@(evalInstr prog . readInstr p -> instr)) =
    case step p instr Nothing of
      (newP, Nothing, updates) -> go newP (i, o, updateProgram prog updates)
      (newP, Just output, updates) ->
        go newP (i, o `Vector.snoc` output, updateProgram prog updates)

-- run with a single input until that input has been read or
-- the program terminates
runInput :: Int -> Program -> Maybe (Pointer, Program)
runInput input initialProg = go 0 initialProg
 where
  go :: Pointer -> Program -> Maybe (Pointer, Program)
  go _ (Vector.null -> True     ) = Nothing
  go p (readInstr p -> Terminate) = Nothing
  go p prog@(evalInstr prog . readInstr p -> instr@(Input _)) =
    let (newP, _, updates) = step p instr (Just input)
    in  Just (newP, (updateProgram prog updates))
  go p prog@(evalInstr prog . readInstr p -> instr) =
    case step p instr Nothing of
      (newP, Nothing, updates) -> go newP (updateProgram prog updates)
      (_   , Just _ , _      ) -> error "unexpected output"

-- run with a single input until a single output is received or
-- the program terminates
runPropagate :: Int -> (Pointer, Program) -> Maybe (Int, Pointer, Program)
runPropagate input (initialP, initialProg) = go initialP initialProg
 where
  go :: Pointer -> Program -> Maybe (Int, Pointer, Program)
  go _ (Vector.null -> True     ) = Nothing
  go p (readInstr p -> Terminate) = Nothing
  go p prog@(evalInstr prog . readInstr p -> instr@(Input _)) =
    let (newP, _, updates) = step p instr (Just input)
    in  go newP (updateProgram prog updates)
  go p prog@(evalInstr prog . readInstr p -> instr) =
    case step p instr Nothing of
      (newP, Nothing    , updates) -> go newP (updateProgram prog updates)
      (newP, Just output, updates) -> Just (output, newP, updateProgram prog updates)

readArg :: Program -> Pointer -> Offset -> Int -> Mode
readArg prog p offset@(Offset o) codes = case (codes `quot` (10 ^ (o - 1))) `rem` 10 of
        0 -> Position (readPointer prog p offset)
        1 -> Immediate (readValue prog p offset)
        n -> error ("unknown mode code: " ++ show n)

readPointer :: Program -> Pointer -> Offset -> Pointer
readPointer prog p offset =
  Pointer $ readValue prog p offset

readValue :: Program -> Pointer -> Offset -> Int
readValue prog (Pointer p) (Offset o) = case prog !? (p + o) of
  Just newP -> newP
  Nothing ->
    error $ "no value at position: " ++ show p ++ " full prog: " ++ show prog

readInstr :: Pointer -> Program -> Instr Mode
readInstr p prog = case (readValue prog p (Offset 0)) `quotRem` 100 of
  (codes, 1) ->
    Add (readArg prog p 1 codes) (readArg prog p 2 codes) (readPointer prog p 3)
  (codes, 2) ->
    Mul (readArg prog p 1 codes) (readArg prog p 2 codes) (readPointer prog p 3)
  (codes, 3) -> Input (readPointer prog p 1)
  (codes, 4) -> Output (readArg prog p 1 codes)
  (codes, 5) -> JumpIfTrue (readArg prog p 1 codes) (readArg prog p 2 codes)
  (codes, 6) -> JumpIfFalse (readArg prog p 1 codes) (readArg prog p 2 codes)
  (codes, 7) -> LessThan (readArg prog p 1 codes)
                         (readArg prog p 2 codes)
                         (readPointer prog p 3)
  (codes, 8) -> Equals (readArg prog p 1 codes)
                       (readArg prog p 2 codes)
                       (readPointer prog p 3)
  (_    , 99) -> (Terminate)
  (codes, n ) -> error
    (  "unknown op code: '"
    ++ show n
    ++ "' with positional codes: '"
    ++ show codes
    ++ "' at position: '"
    ++ show p
    ++ "' and program: "
    ++ show prog
    )

parse :: Text -> Vector Int
parse s = Vector.fromList $ read @Int . Text.unpack <$> Text.splitOn "," s
