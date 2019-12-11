module IntCode
  ( parse
  , run
  , runInput
  , runOutput
  , runPropagate
  , ProgState
  , Pointer(..)
  , RelativeBase(..)
  )
where

import           Data.Bifunctor                           ( Bifunctor
                                                          , bimap
                                                          )
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
  deriving newtype (Show, Num)

newtype Offset = Offset Int
  deriving newtype (Show, Num)

newtype RelativeBase = RelativeBase Pointer
  deriving newtype (Show, Num)

type ProgState = (RelativeBase, Pointer, Program)

type Input = Vector Int
type Output = Vector Int

data Mode = Position Pointer | Immediate Int | Relative Offset
  deriving (Show)

data WriteLocation = WritePosition Pointer | WriteRelative Offset
  deriving (Show)

data Instr arg write where
  Add ::arg -> arg -> write -> Instr arg write
  Mul ::arg -> arg -> write -> Instr arg write
  Input ::write -> Instr arg write
  Output ::arg -> Instr arg write
  JumpIfTrue ::arg -> arg -> Instr arg write
  JumpIfFalse ::arg -> arg -> Instr arg write
  LessThan ::arg -> arg -> write -> Instr arg write
  Equals ::arg -> arg -> write -> Instr arg write
  AdjustBase ::arg -> Instr arg write
  Terminate ::Instr arg write

deriving stock instance (Show a, Show b) => Show (Instr a b)

instance Bifunctor Instr where
  bimap f g (Add arg1 arg2 wr     ) = Add (f arg1) (f arg2) (g wr)
  bimap f g (Mul arg1 arg2 wr     ) = Mul (f arg1) (f arg2) (g wr)
  bimap _ g (Input  wr            ) = Input (g wr)
  bimap f _ (Output arg1          ) = Output (f arg1)
  bimap f _ (JumpIfTrue  arg1 arg2) = JumpIfTrue (f arg1) (f arg2)
  bimap f _ (JumpIfFalse arg1 arg2) = JumpIfFalse (f arg1) (f arg2)
  bimap f g (LessThan arg1 arg2 wr) = LessThan (f arg1) (f arg2) (g wr)
  bimap f g (Equals   arg1 arg2 wr) = Equals (f arg1) (f arg2) (g wr)
  bimap f _ (AdjustBase arg1      ) = AdjustBase (f arg1)
  bimap _ _ Terminate               = Terminate

incPointer :: Pointer -> Offset -> Pointer
incPointer (Pointer p) (Offset o) = Pointer (p + o)

incBase :: RelativeBase -> Offset -> RelativeBase
incBase (RelativeBase r) offset = RelativeBase (incPointer r offset)

updateProgram :: Program -> Updates -> Program
updateProgram prog updates = prog // ((\(Pointer p, v) -> (p, v)) <$> updates)

reserveMemory :: Program -> Int -> Program
reserveMemory prog size =
  prog Vector.++ Vector.replicate (size - Vector.length prog) 0

evalArg :: Program -> RelativeBase -> Mode -> Int
evalArg prog _                (Position  p) = readValue prog p 0
evalArg _    _                (Immediate i) = i
evalArg prog (RelativeBase b) (Relative  r) = readValue prog b r

evalWrite :: RelativeBase -> WriteLocation -> Pointer
evalWrite _                (WritePosition p) = p
evalWrite (RelativeBase b) (WriteRelative r) = incPointer b r

-- Can't just use Functor as write location arguments must be positions
evalInstr
  :: Program -> RelativeBase -> Instr Mode WriteLocation -> Instr Int Pointer
evalInstr prog b = bimap (evalArg prog b) (evalWrite b)

-- Step once the values of instructions have been found
step
  :: Pointer
  -> Instr Int Pointer
  -> Maybe Int
  -> (Offset, Pointer, Maybe Int, Updates)
step p Terminate _ = (0, p, Nothing, [])
step p (Add arg1 arg2 arg3) _ =
  (0, incPointer p 4, Nothing, [(arg3, arg1 + arg2)])
step p (Mul arg1 arg2 arg3) _ =
  (0, incPointer p 4, Nothing, [(arg3, arg1 * arg2)])
step p (Input arg1) (Just input) =
  (0, incPointer p 2, Nothing, [(arg1, input)])
step p (Output arg1) _ = (0, incPointer p 2, Just arg1, [])
step p (JumpIfTrue arg1 arg2) _ =
  (0, if arg1 /= 0 then Pointer arg2 else incPointer p 3, Nothing, [])
step p (JumpIfFalse arg1 arg2) _ =
  (0, if arg1 == 0 then Pointer arg2 else incPointer p 3, Nothing, [])
step p (LessThan arg1 arg2 arg3) _ =
  (0, incPointer p 4, Nothing, [(arg3, if arg1 < arg2 then 1 else 0)])
step p (Equals arg1 arg2 arg3) _ =
  (0, incPointer p 4, Nothing, [(arg3, if arg1 == arg2 then 1 else 0)])
step p (AdjustBase arg1) _ = (Offset arg1, incPointer p 2, Nothing, [])

run :: Program -> Input -> (Output, Program)
run initialProg input =
  let (_, o, finalProg) =
          go 0 0 (input, Vector.empty, reserveMemory initialProg 2048)
  in  (o, finalProg)
 where
  go
    :: RelativeBase
    -> Pointer
    -> (Input, Output, Program)
    -> (Input, Output, Program)
  go _ _ (i, o, prog@(Vector.null -> True)     ) = (i, o, prog)
  go _ p (i, o, prog@(readInstr p -> Terminate)) = (i, o, prog)
  go b p (i, o, prog@(evalInstr prog b . readInstr p -> instr@(Input _))) =
    let (bOffset, newP, _, updates) = step p instr (Just (Vector.head i))
    in  go (incBase b bOffset)
           newP
           (Vector.tail i, o, updateProgram prog updates)
  go b p (i, o, prog@(evalInstr prog b . readInstr p -> instr)) =
    case step p instr Nothing of
      (bOffset, newP, Nothing, updates) ->
        go (incBase b bOffset) newP (i, o, updateProgram prog updates)
      (bOffset, newP, Just output, updates) -> go
        (incBase b bOffset)
        newP
        (i, o `Vector.snoc` output, updateProgram prog updates)

-- run with a single input until that input has been read or
-- the program terminates
runInput :: Int -> ProgState -> Maybe ProgState
runInput input (initialB, initialP, initialProg) = go
  initialB
  initialP
  (reserveMemory initialProg 2048)
 where
  go :: RelativeBase -> Pointer -> Program -> Maybe ProgState
  go _ _ (Vector.null -> True     ) = Nothing
  go _ p (readInstr p -> Terminate) = Nothing
  go b p prog@(evalInstr prog b . readInstr p -> instr@(Input _)) =
    let (bOffset, newP, _, updates) = step p instr (Just input)
    in  Just (incBase b bOffset, newP, updateProgram prog updates)
  go b p prog@(evalInstr prog b . readInstr p -> instr) =
    case step p instr Nothing of
      (bOffset, newP, Nothing, updates) ->
        go (incBase b bOffset) newP (updateProgram prog updates)
      (_, _, Just _, _) -> error "unexpected output"

-- run with a single input until a single output is received or
-- the program terminates
runOutput :: ProgState -> Maybe (Int, ProgState)
runOutput (initialB, initialP, initialProg) = go
  initialB
  initialP
  (reserveMemory initialProg 2048)
 where
  go :: RelativeBase -> Pointer -> Program -> Maybe (Int, ProgState)
  go _ _ (Vector.null -> True     ) = Nothing
  go _ p (readInstr p -> Terminate) = Nothing
  go b p prog@(evalInstr prog b . readInstr p -> instr) =
    case step p instr Nothing of
      (bOffset, newP, Nothing, updates) ->
        go (incBase b bOffset) newP (updateProgram prog updates)
      (bOffset, newP, Just output, updates) ->
        Just (output, (incBase b bOffset, newP, updateProgram prog updates))

-- run with a single input until a single output is received or
-- the program terminates
runPropagate :: Int -> ProgState -> Maybe (Int, ProgState)
runPropagate input state = runInput input state >>= runOutput

readArg :: Program -> Pointer -> Offset -> Int -> Mode
readArg prog p offset@(Offset o) codes =
  case (codes `quot` (10 ^ (o - 1))) `rem` 10 of
    0 -> Position (Pointer $ readValue prog p offset)
    1 -> Immediate (readValue prog p offset)
    2 -> Relative (Offset $ readValue prog p offset)
    n -> error ("unknown mode code: " ++ show n)

readWriteLocation :: Program -> Pointer -> Offset -> Int -> WriteLocation
readWriteLocation prog p offset codes = case readArg prog p offset codes of
  Position p -> WritePosition p
  Relative r -> WriteRelative r
  _          -> error "cannot write to non positional or relative argument"

readValue :: Program -> Pointer -> Offset -> Int
readValue prog (Pointer p) (Offset o) = case prog !? (p + o) of
  Just newP -> newP
  Nothing ->
    error $ "no value at position: " ++ show p ++ " full prog: " ++ show prog

readInstr :: Pointer -> Program -> Instr Mode WriteLocation
readInstr p prog = case readValue prog p (Offset 0) `quotRem` 100 of
  (codes, 1) -> Add (readArg prog p 1 codes)
                    (readArg prog p 2 codes)
                    (readWriteLocation prog p 3 codes)
  (codes, 2) -> Mul (readArg prog p 1 codes)
                    (readArg prog p 2 codes)
                    (readWriteLocation prog p 3 codes)
  (codes, 3) -> Input (readWriteLocation prog p 1 codes)
  (codes, 4) -> Output (readArg prog p 1 codes)
  (codes, 5) -> JumpIfTrue (readArg prog p 1 codes) (readArg prog p 2 codes)
  (codes, 6) -> JumpIfFalse (readArg prog p 1 codes) (readArg prog p 2 codes)
  (codes, 7) -> LessThan (readArg prog p 1 codes)
                         (readArg prog p 2 codes)
                         (readWriteLocation prog p 3 codes)
  (codes, 8) -> Equals (readArg prog p 1 codes)
                       (readArg prog p 2 codes)
                       (readWriteLocation prog p 3 codes)
  (codes, 9 ) -> AdjustBase (readArg prog p 1 codes)
  (_    , 99) -> Terminate
  (codes, n ) -> error
    (  "unknown op code: '"
    ++ show n
    ++ "' with positional codes: '"
    ++ show codes
    ++ "' at position: '"
    ++ show p
    )

parse :: Text -> Vector Int
parse s = Vector.fromList $ read @Int . Text.unpack <$> Text.splitOn "," s
