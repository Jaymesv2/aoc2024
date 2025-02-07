
{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day17 (day17) where

import Solver
import Util.Parsers
import Text.Parsec

import Data.Text (Text)
import Data.Bits

day17 :: Solver
day17 = mkParsecSolver 17 "Chronospatial Computer" () parser solver


runProgram :: [Instruction] -> (Int,Int,Int) -> [Int]
runProgram program = helper 0 
    where
        helper :: Int -> (Int,Int,Int)-> [Int]
        helper pc regFile
            | pc >= length program = []
            | otherwise = case interpretInstr program pc regFile of
                (pc', regFile', Just x) -> x:helper pc' regFile'
                (pc', regFile', Nothing) -> helper pc' regFile'


interpretInstr :: [Instruction] -> Int -> (Int,Int,Int) -> (Int, (Int,Int,Int), Maybe Int)
interpretInstr program pc regs@(a,b,c) = case program !! pc of
    Jnz reg to -> (if readReg regs reg == 0 then pc+1 else to `div` 2, (a,b,c), Nothing )
    Write r e -> (pc+1, writeReg regs r $ evalExpr regs e, Nothing)
    Out e -> (pc+1, (a,b,c), Just (evalExpr regs e))

writeReg :: (Int,Int,Int) -> Register -> Int -> (Int,Int,Int)
writeReg (_,b,c) A x = (x,b,c)
writeReg (a,_,c) B x = (a,x,c)
writeReg (a,b,_) C x = (a,b,x)

readReg :: (Int,Int,Int) -> Register -> Int
readReg (a,_,_) A = a
readReg (_,b,_) B = b
readReg (_,_,c) C = c

evalExpr :: (Int,Int,Int) -> Expr Operand -> Int
evalExpr _ (Val (Literal n)) = n
evalExpr regs (Val (Register r)) = readReg regs r
evalExpr regs (EXor e1 e2)  = evalExpr regs e1 `xor` evalExpr regs e2
evalExpr regs (ShiftR e1 e2) = evalExpr regs e1 `shiftR` evalExpr regs e2
evalExpr regs (Trunc e) = evalExpr regs e .&. 7


solver :: ((Int,Int,Int),[Int]) -> (String,Int)
solver (initRegisters, originalProgram) = 
    (part1,part2)
    where
        newProgram :: [Instruction]
        newProgram = toInstruction <$> pairs originalProgram
        part1 = show $ runProgram newProgram initRegisters
        part2 = head $ solvePart2 originalProgram newProgram



solvePart2 :: [Int] -> [Instruction] -> [Int]
solvePart2 oProgram program = helper roProgram 0
    where   
        roProgram = reverse oProgram
            
        helper :: [Int] -> Int -> [Int]
        helper [] a = [a]
        helper (o:os) currentA = do
            newA <- aMustEqualAtEnd currentA o
            helper os newA

        aMustEqualAtEnd :: Int -> Int -> [Int]
        aMustEqualAtEnd futureA out = do
            lowBits <- [0..7]
            let oldA = lowBits .|. (futureA `shiftL` 3)
                ((computedA, _,_), outP) = interpret program (oldA,0,0) Nothing
            if computedA == futureA && outP == out 
                then pure oldA
                else []

        interpret :: [Instruction] -> (Int,Int,Int) -> Maybe Int -> ((Int,Int,Int),Int)
        interpret [] regs (Just d) = (regs,d)
        interpret [] _ Nothing = error "program didnt output"
        interpret (instr:pgrm) regFile out = case instr of 
            Jnz _ _ -> interpret pgrm regFile out
            Write r e -> interpret pgrm (writeReg regFile r $ evalExpr regFile e) out
            Out e -> interpret pgrm regFile (Just (evalExpr regFile e))

parser :: Parsec Text () ((Int,Int,Int), [Int])
parser = do
    registers <- (,,) <$> register <*> register <*> register -- could also do this with liftA3 
    _ <- string "\nProgram: "
    program <- decimal `sepBy1` char ','
    pure (registers,  program)
    where
        register = string "Register " *> anyChar *> string ": " *> decimal <* char '\n'



pairs :: [a] -> [(a,a)]
pairs (a:b:xs) = (a,b):pairs xs
pairs _ = []

data Register = A | B | C deriving stock (Show, Eq)
data Operand = Register Register | Literal Int deriving stock (Show, Eq)

data Instruction
    = Write Register (Expr Operand)
    | Out (Expr Operand) -- 5
    | Jnz Register Int
    deriving stock (Show, Eq)

data Expr a
    = Val a
    | EXor (Expr a ) (Expr a)
    | ShiftR (Expr a) (Expr a)
    | Trunc (Expr a)
    deriving stock (Show, Eq, Functor)
    


toInstruction :: (Int,Int) -> Instruction
toInstruction (0,n) = Write A $ ShiftR (Val $ Register A) (comboToOperand n)
toInstruction (1,n) = Write B $ EXor (Val $ Register B) (Val $ Literal n)
toInstruction (2,n) = Write B $ Trunc (comboToOperand n)
toInstruction (3,n) = Jnz A n
toInstruction (4,_) = Write B $ EXor (Val $ Register B) (Val $ Register C)
toInstruction (5,n) = Out (Trunc (comboToOperand n))
toInstruction (6,n) = Write B $ ShiftR (Val $ Register A) (comboToOperand n)
toInstruction (7,n) = Write C $ ShiftR (Val $ Register A) (comboToOperand n)
toInstruction _ = error "bad bad bad"


comboToOperand :: Int -> Expr Operand
comboToOperand 0 = Val $ Literal 0
comboToOperand 1 = Val $ Literal 1
comboToOperand 2 = Val $ Literal 2
comboToOperand 3 = Val $ Literal 3
comboToOperand 4 = Val $ Register A
comboToOperand 5 = Val $ Register B
comboToOperand 6 = Val $ Register C
comboToOperand _ = error "invalid combo op"

