module IntCode (parse, run) where

import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import           Data.Vector                              ( (!?)
                                                          , (//)
                                                          , Vector
                                                          )
import qualified Data.Vector                   as Vector

type Program = Vector Int
type Pointer = Int
type Offset = Int
type Input = Vector Int
type Output = Vector Int

data Mode = Position | Immediate

data Instr where
  Add :: (Int, Mode) -> (Int, Mode) -> (Int, Mode) -> Instr
  Mul :: (Int, Mode) -> (Int, Mode) -> (Int, Mode) -> Instr
  Input :: (Int, Mode) -> Instr
  Output :: (Int, Mode) -> Instr
  JumpIfTrue :: (Int, Mode) -> (Int, Mode) -> Instr
  JumpIfFalse :: (Int, Mode) -> (Int, Mode) -> Instr
  LessThan :: (Int, Mode) -> (Int, Mode) -> (Int, Mode) -> Instr
  Equals :: (Int, Mode) -> (Int, Mode) -> (Int, Mode) -> Instr
  Terminate :: Instr

run :: Program -> Input -> (Output, Program)
run initialProg input =
  let (_, o, finalProg) = go 0 (input, Vector.empty, initialProg)
  in  (o, finalProg)
 where
  go :: Pointer -> (Input, Output, Program) -> (Input, Output, Program)
  go _ (i, o, prog@(Vector.null -> True)          ) = (i, o, prog)
  go p (i, o, prog@(readInstr p -> (_, Terminate))) = (i, o, prog)
  go p (i, o, prog@(readInstr p -> (offset, Add arg1 arg2 (instrOut, Position))))
    = go (p + offset)
         (i, o, prog // [(instrOut, evalArg prog arg1 + evalArg prog arg2)])
  go p (i, o, prog@(readInstr p -> (offset, Mul arg1 arg2 (instrOut, Position))))
    = go (p + offset)
         (i, o, prog // [(instrOut, evalArg prog arg1 * evalArg prog arg2)])
  go p (i, o, prog@(readInstr p -> (offset, Input (instrOut, Position)))) = go
    (p + offset)
    (Vector.tail i, o, prog // [(instrOut, Vector.head input)])
  go p (i, o, prog@(readInstr p -> (offset, Output arg1))) =
    go (p + offset) (i, o `Vector.snoc` evalArg prog arg1, prog)
  go p (i, o, prog@(readInstr p -> (offset, JumpIfTrue arg1 arg2))) = go
    (if evalArg prog arg1 /= 0 then evalArg prog arg2 else p + offset)
    (i, o, prog)
  go p (i, o, prog@(readInstr p -> (offset, JumpIfFalse arg1 arg2))) = go
    (if evalArg prog arg1 == 0 then evalArg prog arg2 else p + offset)
    (i, o, prog)
  go p (i, o, prog@(readInstr p -> (offset, LessThan arg1 arg2 (instrOut, Position))))
    = go
      (p + offset)
      ( i
      , o
      , prog
        // [(instrOut, if evalArg prog arg1 < evalArg prog arg2 then 1 else 0)]
      )
  go p (i, o, prog@(readInstr p -> (offset, Equals arg1 arg2 (instrOut, Position))))
    = go
      (p + offset)
      ( i
      , o
      , prog
        // [(instrOut, if evalArg prog arg1 == evalArg prog arg2 then 1 else 0)]
      )
  go _ (i, o, prog) = (i, o, prog)

evalArg :: Program -> (Int, Mode) -> Int
evalArg prog (p, Position) = case prog !? p of
  Just arg -> arg
  Nothing  -> error $ "no entry at position: " ++ show p
evalArg _ (n, Immediate) = n

readArg :: Program -> Pointer -> Offset -> Int -> (Int, Mode)
readArg prog p offset codes =
  let code = case (codes `quot` (10 ^ (offset - 1))) `rem` 10 of
        0 -> Position
        1 -> Immediate
        n -> error ("unknown mode code: " ++ show n)
  in  case prog !? (p + offset) of
        Just newP -> (newP, code)
        Nothing ->
          error
            $  "no arg at position: "
            ++ show (p + offset)
            ++ " full prog: "
            ++ show prog

readInstr :: Pointer -> Program -> (Offset, Instr)
readInstr p prog
  = let opcode = case prog !? p of
          Just fullcode -> fullcode
          Nothing       -> error $ "no code at pointer position: " ++ show p
    in
      case opcode `quotRem` 100 of
        (codes, 1) ->
          ( 4
          , Add (readArg prog p 1 codes)
                (readArg prog p 2 codes)
                (readArg prog p 3 codes)
          )
        (codes, 2) ->
          ( 4
          , Mul (readArg prog p 1 codes)
                (readArg prog p 2 codes)
                (readArg prog p 3 codes)
          )
        (codes, 3) -> (2, Input (readArg prog p 1 codes))
        (codes, 4) -> (2, Output (readArg prog p 1 codes))
        (codes, 5) ->
          (3, JumpIfTrue (readArg prog p 1 codes) (readArg prog p 2 codes))
        (codes, 6) ->
          (3, JumpIfFalse (readArg prog p 1 codes) (readArg prog p 2 codes))
        (codes, 7) ->
          ( 4
          , LessThan (readArg prog p 1 codes)
                     (readArg prog p 2 codes)
                     (readArg prog p 3 codes)
          )
        (codes, 8) ->
          ( 4
          , Equals (readArg prog p 1 codes)
                   (readArg prog p 2 codes)
                   (readArg prog p 3 codes)
          )
        (_    , 99) -> (0, Terminate)
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
