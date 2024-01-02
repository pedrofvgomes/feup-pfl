import Data.List (sort, intercalate)
import Data.Char (isDigit, isAlphaNum, isSpace, isLower)

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- storing as a list of strings (either numeric strings or boolean strings, to be converted later)
type Stack = [String]

-- storing as a list of pairs of strings (either numeric strings or boolean strings, to be converted later)
type State = [(String, String)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," stack

state2Str :: State -> String
state2Str = intercalate "," . map (\(var, val) -> var ++ "=" ++ val) . sort

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:insts, stack, state) = case inst of
  Push n -> run (insts, (show n):stack, state)
  Add -> applyBinaryOperation (+)
  Mult -> applyBinaryOperation (*)
  Sub -> applyBinaryOperation (-)
  Tru -> pushValue "True"
  Fals -> pushValue "False"
  Equ -> case stack of 
    x:y:rest -> if (elem x ["True", "False"] && elem y ["True", "False"]) || (all isDigit x && all isDigit y || x !! 0 == '-' && all isDigit (drop 1 x) && y !! 0 == '-' && all isDigit (drop 1 y))
      then run (insts, (show (x == y)):rest, state)
      else error "Run-time error"
    _ -> error "Run-time error"
  Le -> case stack of
    x:y:rest -> if all isDigit x && all isDigit y || x !! 0 == '-' && all isDigit (drop 1 x) && y !! 0 == '-' && all isDigit (drop 1 y)
      then run (insts, (show ((read x :: Integer) <= (read y :: Integer))):rest, state)
      else error "Run-time error"
    _ -> error "Run-time error"
  And -> applyLogicOperation (&&)
  Neg -> applyNegationOperation
  Fetch var -> runFetch var
  Store var -> runStore var
  Noop -> run (insts, stack, state)
  Branch c1 c2 -> runBranch c1 c2
  Loop c1 c2 -> runLoop c1 c2
  where
    -- Funções auxiliares para as instruções
    applyBinaryOperation op = case stack of
      x:y:rest -> run (insts, (show (op (read x) (read y))):rest, state)
      _ -> error "Run-time error"

    applyLogicOperation op = case stack of
      x:y:rest -> if elem x ["True", "False"] && elem y ["True", "False"] then run (insts, (show (op (read x) (read y))):rest, state) else error "Run-time error"
      _ -> error "Run-time error"

    applyNegationOperation = case stack of
      x:rest -> if elem x ["True", "False"] then run (insts, (show (not (read x))):rest, state) else error "Run-time error"
      _ -> error "Run-time error"

    pushValue val = run (insts, val:stack, state)

    runFetch var = case lookup var state of
      Just val -> run (insts, val:stack, state)
      Nothing -> error "Run-time error"

    runStore var =
      case stack of
        val:rest ->
          case lookup var state of
            Just _  -> run (insts, rest, updateState var val state)
            Nothing -> run (insts, rest, (var, val):state)
        _ -> error "Run-time error"
      where
        updateState :: String -> String -> State -> State
        updateState key newVal [] = [(key, newVal)]
        updateState key newVal ((k, v):rest)
          | key == k  = (key, newVal) : rest
          | otherwise = (k, v) : updateState key newVal rest

    runBranch c1 c2 = case stack of
      val:rest -> if val == "True" then run (c1 ++ insts, rest, state) else if val == "False" then run (c2 ++ insts, rest, state) else error "Run-time error"
      _ -> error "Run-time error"

    runLoop c1 c2 = run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ insts, stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- testRuntimeError1 = testAssembler [Fetch "undefined_var"]
-- testRuntimeError2 = testAssembler [Tru, Push 5, And]
-- testRuntimeError3 = testAssembler [Push 10, Branch [Push 1] [Push 2]]
-- testRuntimeError4 = testAssembler [Loop [] [], Fetch "x"]
-- testRuntimeError5 = testAssembler [Loop [Push 1, Push 2] []]
-- testRuntimeError6 = testAssembler [Tru, Neg, Push 5, And]
-- testRuntimeError8 = testAssembler [Loop [] [Push 1, Push 2], Fetch "x"]
-- testRuntimeError10 = testAssembler [Push 5, Neg]
-- testRuntimeError11 = testAssembler [Push 5, Loop [Push 1, Push 2] [Push 3, Push 4]]
-- testRuntimeError12 = testAssembler [Push 5, Branch [Push 1] [Push 2]]
-- testRuntimeError13 = testAssembler [Push 5, Fetch "undefined_var", Push 10, Store "var", Fetch "var"]
-- testRuntimeError14 = testAssembler [Loop [] [], And]
-- testRuntimeError15 = testAssembler [Branch [] []]
-- testRuntimeError16 = testAssembler [Noop, Fetch "x"]



-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program
data Aexp = 
  Var String | Num Integer | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp
  deriving (Show, Eq)

data Bexp = 
  TrueB | FalseB | AndB Bexp Bexp | EqA Aexp Aexp | EqB Bexp Bexp | LeB Aexp Aexp | NotB Bexp
  deriving (Show, Eq)

data Stm = Assign String Aexp | If Bexp [Stm] [Stm] | While Bexp [Stm] | Seq [Stm]
  deriving (Show, Eq)

type Program = [Stm]

compA :: Aexp -> Code
compA exp = case exp of
  Var x -> [Fetch x]
  Num n -> [Push n]
  AddA a1 a2 -> compA a1 ++ compA a2 ++ [Add]
  SubA a1 a2 -> compA a1 ++ compA a2 ++ [Sub]
  MultA a1 a2 -> compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB exp = case exp of
  TrueB -> [Tru]
  FalseB -> [Fals]
  AndB b1 b2 -> compB b1 ++ compB b2 ++ [And]
  EqA a1 a2 -> compA a1 ++ compA a2 ++ [Equ]
  EqB b1 b2 -> compB b1 ++ compB b2 ++ [Equ]
  LeB a1 a2 -> compA a1 ++ compA a2 ++ [Le]
  NotB b -> compB b ++ [Neg]


compile :: Program -> Code
compile program = let code = compileStm program in code

compileStm :: [Stm] -> Code
compileStm [] = []
compileStm (stm:rest) = case stm of 
  Assign var a -> compA a ++ [Store var] ++ compileStm rest
  If b s1 s2 -> compB b ++ [Branch (compileStm s1) (compileStm s2)] ++ compileStm rest
  While b s -> [Loop (compB b) (compileStm s)] ++ compileStm rest
  Seq s -> compileStm s ++ compileStm rest


data Token = 
  TokenAdd | TokenMult | TokenSub | TokenOpenP | TokenCloseP | TokenIf 
  | TokenThen | TokenElse | TokenAssign | TokenWhile | TokenDo | TokenTrue 
  | TokenFalse | TokenAnd | TokenNot | TokenEqA | TokenEqB | TokenLe | TokenSemiColon
  | TokenNum Integer | TokenVar String
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : rest) = TokenAdd : lexer rest
lexer ('*' : rest) = TokenMult : lexer rest
lexer ('-' : rest) = TokenSub : lexer rest
lexer ('(' : rest) = TokenOpenP : lexer rest
lexer (')' : rest) = TokenCloseP : lexer rest
lexer ('n' : 'o' : 't' : rest) = TokenNot : lexer rest
lexer ('a' : 'n' : 'd' : rest) = TokenAnd : lexer rest
lexer ('i' : 'f' : rest) = TokenIf : lexer rest
lexer ('t' : 'h' : 'e' : 'n' : rest) = TokenThen : lexer rest
lexer ('e' : 'l' : 's' : 'e' : rest) = TokenElse : lexer rest
lexer ('w' : 'h' : 'i' : 'l' : 'e' : rest) = TokenWhile : lexer rest
lexer ('d' : 'o' : rest) = TokenDo : lexer rest
lexer ('=' : '=' : rest) = TokenEqA : lexer rest
lexer ('=' : rest) = TokenEqB : lexer rest
lexer ('<' : '=' : rest) = TokenLe : lexer rest
lexer (':' : '=' : rest) = TokenAssign : lexer rest
lexer ('T' : 'r' : 'u' : 'e' : rest) = TokenTrue : lexer rest
lexer ('F' : 'a' : 'l' : 's' : 'e' : rest) = TokenFalse : lexer rest
lexer (';' : rest) = TokenSemiColon : lexer rest
lexer (c : rest)
  | isSpace c = lexer rest
  | isDigit c = TokenNum (read num) : lexer rest'
  | isLower c = TokenVar var : lexer rest''
  | otherwise = error ("Bad character: " ++ [c])
  where 
    (num, rest') = span isDigit (c:rest)
    (var, rest'') = span isAlphaNum (c:rest)

token2Stm :: [Token] -> [Stm]
token2Stm [] = []
token2Stm (TokenSemiColon : rest) = token2Stm rest
token2Stm (TokenVar var : TokenAssign : rest) = Assign var (token2Aexp rest) : token2Stm rest
token2Stm (TokenIf : rest) = If (token2Bexp rest) (token2Stm thenn) (token2Stm elsee) : token2Stm rest'
  where 
    (bexp, withThen) = break (== TokenThen) rest
    afterThen = tail withThen
    (thenn, withElse) = if head afterThen == TokenOpenP then betweenParentheses afterThen else break (== TokenSemiColon) afterThen
    afterElse = if (withElse !! 0) == TokenSemiColon then drop 2 withElse else tail withElse
    (elsee, rest') = if (afterElse !! 0) == TokenOpenP then betweenParentheses afterElse else break (== TokenSemiColon) afterElse
token2Stm (TokenWhile : rest) = While (token2Bexp bexp) (token2Stm stm) : token2Stm rest
  where 
    (bexp, withDo) = break (== TokenDo) rest
    (stm, rest) = if withDo !! 1 == TokenOpenP then betweenParentheses (tail withDo) else break (== TokenSemiColon) (tail withDo)
token2Stm _ = error "Syntax error"

-- parse :: String -> Program
parse = undefined

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")