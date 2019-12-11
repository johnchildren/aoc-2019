module IntCode
  ( parse
  , run
  , runInput
  , runPropagate
  , ProgState
  , Pointer(..)
  , RelativeBase(..)
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

newtype RelativeBase = RelativeBase Pointer
  deriving (Show, Num)

type ProgState = (RelativeBase, Pointer, Program)

type Input = Vector Int
type Output = Vector Int

data Mode = Position Pointer | Immediate Int | Relative Offset
  deriving (Show)

data WriteLocation = WritePosition Pointer | WriteRelative Offset
  deriving (Show)

-- TODO: make this a bifunctor
data Instr a where
  Add ::a -> a -> WriteLocation -> Instr a
  Mul ::a -> a -> WriteLocation -> Instr a
  Input ::WriteLocation -> Instr a
  Output ::a -> Instr a
  JumpIfTrue ::a -> a -> Instr a
  JumpIfFalse ::a -> a -> Instr a
  LessThan ::a -> a -> WriteLocation -> Instr a
  Equals ::a -> a -> WriteLocation -> Instr a
  AdjustBase ::a -> Instr a
  Terminate ::Instr a

deriving instance Functor Instr
deriving instance (Show a) => Show (Instr a)

incPointer :: Pointer -> Offset -> Pointer
incPointer (Pointer p) (Offset o) = Pointer (p + o)

updateProgram :: Program -> Updates -> Program
updateProgram prog updates = prog // ((\(Pointer p, v) -> (p, v)) <$> updates)

reserveMemory :: Program -> Int -> Program
reserveMemory prog size =
  prog Vector.++ Vector.replicate (size - Vector.length prog) 0

evalArg :: Program -> RelativeBase -> Mode -> Int
evalArg prog _                (Position  p) = readValue prog p 0
evalArg _    _                (Immediate i) = i
evalArg prog (RelativeBase b) (Relative  r) = readValue prog b r

-- Can't just use Functor as write location arguments must be positions
evalInstr :: Program -> RelativeBase -> Instr Mode -> Instr Int
evalInstr prog b = fmap (evalArg prog b)

-- Step once the values of instructions have been found
step
  :: RelativeBase
  -> Pointer
  -> Instr Int
  -> Maybe Int
  -> (RelativeBase, Pointer, Maybe Int, Updates)
step b p Terminate _ = (b, p, Nothing, [])

step b p (Add arg1 arg2 (WritePosition arg3)) _ =
  (b, incPointer p 4, Nothing, [(arg3, arg1 + arg2)])
step b p (Mul arg1 arg2 (WritePosition arg3)) _ =
  (b, incPointer p 4, Nothing, [(arg3, arg1 * arg2)])

step b@(RelativeBase base) p (Add arg1 arg2 (WriteRelative arg3)) _ =
  (b, incPointer p 4, Nothing, [(incPointer base arg3, arg1 + arg2)])
step b@(RelativeBase base) p (Mul arg1 arg2 (WriteRelative arg3)) _ =
  (b, incPointer p 4, Nothing, [(incPointer base arg3, arg1 * arg2)])

step b p (Input (WritePosition arg1)) (Just input) =
  (b, incPointer p 2, Nothing, [(arg1, input)])
step b@(RelativeBase base) p (Input (WriteRelative arg1)) (Just input) =
  (b, incPointer p 2, Nothing, [(incPointer base arg1, input)])

step b p (Output arg1) _ = (b, incPointer p 2, Just arg1, [])
step b p (JumpIfTrue arg1 arg2) _ =
  (b, if arg1 /= 0 then Pointer arg2 else incPointer p 3, Nothing, [])
step b p (JumpIfFalse arg1 arg2) _ =
  (b, if arg1 == 0 then Pointer arg2 else incPointer p 3, Nothing, [])

step b p (LessThan arg1 arg2 (WritePosition arg3)) _ =
  (b, incPointer p 4, Nothing, [(arg3, if arg1 < arg2 then 1 else 0)])
step b p (Equals arg1 arg2 (WritePosition arg3)) _ =
  (b, incPointer p 4, Nothing, [(arg3, if arg1 == arg2 then 1 else 0)])

step b@(RelativeBase base) p (LessThan arg1 arg2 (WriteRelative arg3)) _ =
  ( b
  , incPointer p 4
  , Nothing
  , [(incPointer base arg3, if arg1 < arg2 then 1 else 0)]
  )
step b@(RelativeBase base) p (Equals arg1 arg2 (WriteRelative arg3)) _ =
  ( b
  , incPointer p 4
  , Nothing
  , [(incPointer base arg3, if arg1 == arg2 then 1 else 0)]
  )

step (RelativeBase base) p (AdjustBase arg1) _ =
  (RelativeBase (incPointer base (Offset arg1)), incPointer p 2, Nothing, [])

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
    let (newB, newP, _, updates) = step b p instr (Just (Vector.head i))
    in  go newB newP (Vector.tail i, o, updateProgram prog updates)
  go b p (i, o, prog@(evalInstr prog b . readInstr p -> instr)) =
    case step b p instr Nothing of
      (newB, newP, Nothing, updates) ->
        go newB newP (i, o, updateProgram prog updates)
      (newB, newP, Just output, updates) ->
        go newB newP (i, o `Vector.snoc` output, updateProgram prog updates)

-- run with a single input until that input has been read or
-- the program terminates
runInput :: Int -> Program -> Maybe ProgState
runInput input initialProg = go 0 0 (reserveMemory initialProg 2048)
 where
  go :: RelativeBase -> Pointer -> Program -> Maybe ProgState
  go b _ (Vector.null -> True     ) = Nothing
  go b p (readInstr p -> Terminate) = Nothing
  go b p prog@(evalInstr prog b . readInstr p -> instr@(Input _)) =
    let (newB, newP, _, updates) = step b p instr (Just input)
    in  Just (newB, newP, updateProgram prog updates)
  go b p prog@(evalInstr prog b . readInstr p -> instr) =
    case step b p instr Nothing of
      (newB, newP, Nothing, updates) ->
        go newB newP (updateProgram prog updates)
      (_, _, Just _, _) -> error "unexpected output"

-- run with a single input until a single output is received or
-- the program terminates
runPropagate :: Int -> ProgState -> Maybe (Int, ProgState)
runPropagate input (initialB, initialP, initialProg) = go
  initialB
  initialP
  (reserveMemory initialProg 2048)
 where
  go :: RelativeBase -> Pointer -> Program -> Maybe (Int, ProgState)
  go _ _ (Vector.null -> True     ) = Nothing
  go _ p (readInstr p -> Terminate) = Nothing
  go b p prog@(evalInstr prog b . readInstr p -> instr@(Input _)) =
    let (newB, newP, _, updates) = step b p instr (Just input)
    in  go newB newP (updateProgram prog updates)
  go b p prog@(evalInstr prog b . readInstr p -> instr) =
    case step b p instr Nothing of
      (newB, newP, Nothing, updates) ->
        go newB newP (updateProgram prog updates)
      (newB, newP, Just output, updates) ->
        Just (output, (newB, newP, updateProgram prog updates))

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

readInstr :: Pointer -> Program -> Instr Mode
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
