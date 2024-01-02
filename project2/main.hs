import Data.List (sort, intercalate)
import Data.Char (isDigit, isAlphaNum, isSpace, isLower)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Stack = [String]

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

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

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
  AddA a1 a2 -> compA a2 ++ compA a1 ++ [Add]
  SubA a1 a2 -> compA a2 ++ compA a1 ++ [Sub]
  MultA a1 a2 -> compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB exp = case exp of
  TrueB -> [Tru]
  FalseB -> [Fals]
  AndB b1 b2 -> compB b2 ++ compB b1 ++ [And]
  EqA a1 a2 -> compA a2 ++ compA a1 ++ [Equ]
  EqB b1 b2 -> compB b2 ++ compB b1 ++ [Equ]
  LeB a1 a2 -> compA a2 ++ compA a1 ++ [Le]
  NotB b -> compB b ++ [Neg]


compile :: Program -> Code
compile [] = []
compile (Assign var a:rest) = compA a ++ [Store var] ++ compile rest
compile (If b s1 s2:rest) = compB b ++ [Branch (compile s1) (compile s2)] ++ compile rest
compile (While b s:rest) = Loop (compB b) (compile s) : compile rest

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
token2Stm ((TokenVar var): TokenAssign : rest) = Assign var (token2Aexp exp) : token2Stm rest'
  where (exp, rest') = break (== TokenSemiColon) rest
token2Stm (TokenIf : tokens) = If (token2Bexp b) (token2Stm thenn) (token2Stm elsee) : token2Stm rest
  where 
    (b, withThen) = break (== TokenThen) tokens
    afterThen = drop 1 withThen
    (thenn, withElse) = if afterThen !! 0 == TokenOpenP then betweenParentheses afterThen else break (== TokenSemiColon) afterThen
    afterElse = if withElse !! 0 == TokenSemiColon then drop 2 withElse else drop 1 withElse
    (elsee, rest) = if afterElse !! 0 == TokenOpenP then betweenParentheses afterElse else break (== TokenSemiColon) afterElse
token2Stm (TokenWhile : tokens) = While (token2Bexp bexp) (token2Stm stm) : token2Stm rest
  where 
    (bexp, withDo) = break (== TokenDo) tokens
    (stm, rest) = if withDo !! 1 == TokenOpenP then betweenParentheses (drop 1 withDo) else break (== TokenSemiColon) (drop 1 withDo)
token2Stm _ = error "Syntax error"

token2Aexp :: [Token] -> Aexp
token2Aexp tokens = 
  case parseSum tokens of 
    (exp, []) -> exp
    (_, _) -> error ("Syntax error")

parseSum :: [Token] -> (Aexp, [Token])
parseSum tokens = 
  case parseMult tokens of
    (e1, TokenAdd : rest) -> case parseSum rest of (e2, rest') -> (AddA e1 e2, rest')
    (e1, TokenSub : rest) -> case parseSum rest of (e2, rest') -> (SubA e1 e2, rest')
    result -> result

parseMult :: [Token] -> (Aexp, [Token])
parseMult tokens = 
  case parseAtom tokens of
    (e1, TokenMult : rest) -> case parseMult rest of (e2, rest') -> (MultA e1 e2, rest')
    result -> result

parseAtom :: [Token] -> (Aexp, [Token])
parseAtom (TokenNum n : rest) = (Num n, rest)
parseAtom (TokenVar var : rest) = (Var var, rest)
parseAtom (TokenOpenP : rest) = case parseSum rest of 
  (e, TokenCloseP : rest') -> (e, rest')
  _ -> error ("Syntax error")
parseAtom _ = error ("Syntax error")

token2Bexp :: [Token] -> Bexp
token2Bexp tokens = 
  case parseAnd tokens of 
    (exp, []) -> exp
    (_, _) -> error ("Syntax error")

parseAnd :: [Token] -> (Bexp, [Token])
parseAnd tokens = 
  case parseEqB tokens of
    (e1, TokenAnd : rest) -> case parseAnd rest of (e2, rest') -> (AndB e1 e2, rest')
    result -> result

parseEqB :: [Token] -> (Bexp, [Token])
parseEqB tokens = 
  case parseNot tokens of 
    (e1, TokenEqB : rest) -> case parseEqB rest of (e2, rest') -> (EqB e1 e2, rest')
    result -> result

parseNot :: [Token] -> (Bexp, [Token])
parseNot (TokenNot : rest) = case parseNot rest of (e, rest) -> (NotB e, rest)
parseNot tokens = parseEqA tokens

parseEqA :: [Token] -> (Bexp, [Token])
parseEqA tokens = 
  case parseSum tokens of 
    (e1, TokenEqA : rest) -> case parseSum rest of (e2, rest') -> (EqA e1 e2, rest')
    result -> parseLe tokens

parseLe :: [Token] -> (Bexp, [Token])
parseLe tokens = case parseSum tokens of
  (e1, TokenLe : rest) -> case parseSum rest of (e2, rest') -> (LeB e1 e2, rest')
  result -> parseParentheses tokens

parseParentheses :: [Token] -> (Bexp, [Token])
parseParentheses (TokenTrue : rest) = (TrueB, rest)
parseParentheses (TokenFalse : rest) = (FalseB, rest)
parseParentheses (TokenOpenP : rest) = case parseAnd rest of (e, TokenCloseP : rest') -> (e, rest')

betweenParentheses :: [Token] -> ([Token], [Token])
betweenParentheses tokens = (elsee, rest)
  where (rest, _, elsee) = betweenParenthesesAux tokens [] []

betweenParenthesesAux :: [Token] -> [Char] -> [Token] -> ([Token], [Char], [Token])
betweenParenthesesAux [] stack r = ([], [], reverse r)
betweenParenthesesAux (TokenOpenP : tokens) stack r = betweenParenthesesAux tokens ('(' : stack) r
betweenParenthesesAux (TokenCloseP : tokens) stack r = betweenParenthesesAux tokens (drop 1 stack) r
betweenParenthesesAux (token : tokens) stack r 
  | null stack = (token:tokens, [], reverse r)
  | otherwise = betweenParenthesesAux tokens stack (token:r)


parse :: String -> Program
parse = token2Stm . lexer

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do
   let result1 = testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
       result2 = testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
       result3 = testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
       result4 = testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
       result5 = testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
       result6 = testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
       result7 = testAssembler [Push (-20),Push (-21), Le] == ("True","")
       result8 = testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
       result9 = testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
       result10 = testParser "x := 5; x := x - 1;" == ("","x=4")
       result12 = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
       result13 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
       result14 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
       result15 = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
   putStrLn ("1 -> " ++ (show result1))
   putStrLn ("2 -> " ++ (show result2))
   putStrLn ("3 -> " ++ (show result3))
   putStrLn ("4 -> " ++ (show result4))
   putStrLn ("5 -> " ++ (show result5))
   putStrLn ("6 -> " ++ (show result6))
   putStrLn ("7 -> " ++ (show result7))
   putStrLn ("8 -> " ++ (show result8))
   putStrLn ("9 -> " ++ (show result9))
   putStrLn ("10 -> " ++ ( show result10))
   putStrLn ("12 -> " ++ ( show result12))
   putStrLn ("13 -> " ++ ( show result13))
   putStrLn ("14 -> " ++ ( show result14))
   putStrLn ("15 -> " ++ ( show result15))