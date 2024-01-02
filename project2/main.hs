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
      x:rest -> run (insts, (show (not (read x))):rest, state)
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
-- ver testRuntimeError3 = testAssembler [Push 10, Branch [Push 1] [Push 2]]
-- testRuntimeError4 = testAssembler [Loop [] [], Fetch "x"]
-- ver testRuntimeError5 = testAssembler [Loop [Push 1, Push 2] []]
-- testRuntimeError6 = testAssembler [Tru, Neg, Push 5, And]
-- ver testRuntimeError7 = testAssembler [Push 10, Store "x", Push 20, Store "x", Fetch "x"]
-- testRuntimeError8 = testAssembler [Loop [] [Push 1, Push 2], Fetch "x"]
-- ver testRuntimeError9 = testAssembler [Push 5, Push "String", Equ]
-- ver testRuntimeError10 = testAssembler [Push 5, Neg]
-- ver testRuntimeError11 = testAssembler [Push 5, Loop [Push 1, Push 2] [Push 3, Push 4]]
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

buildData :: [Token] -> [Stm]
buildData [] = []
buildData (TokenSemiColon:tokens) = buildData tokens
buildData ((TokenVar var):TokenAssign:tokens) = Assign var (buildAexp aexp) : buildData rest
  where (aexp, rest) = break (== TokenSemiColon) tokens

buildData (TokenIf:tokens) = If (buildBexp bexp) (buildData thenTokens) (buildData elseTokens) : buildData rest
    where (bexp, withThenTokens) = break (== TokenThen) tokens
          afterThenTokens = tail withThenTokens
          (thenTokens, withElseTokens) = 
                if head afterThenTokens == TokenOpenP then
                  getBetweenParenTokens afterThenTokens 
                else
                    break (== TokenSemiColon) afterThenTokens
          afterElseTokens =
                if head withElseTokens == TokenSemiColon then
                  drop 2 withElseTokens   -- drop SemiColonTok and ElseTok
                else
                  tail withElseTokens     -- drop ElseTok
          (elseTokens, rest) =
                if head afterElseTokens == TokenOpenP then    -- if parenthesis
                    getBetweenParenTokens afterElseTokens       -- statements between parenthesis
                else
                    break (== TokenSemiColon) afterElseTokens     -- only 1 statement w/o parenthesis

buildData (TokenWhile:tokens) = While (buildBexp bexp) (buildData doTokens) : buildData rest
    where (bexp, withDoTokens) = break (== TokenDo) tokens
          (doTokens, rest) =
                if head (tail withDoTokens) == TokenOpenP then
                    getBetweenParenTokens (tail withDoTokens)
                else
                    break (== TokenSemiColon) (tail withDoTokens)
buildData _ = error "Invalid program on buildData"
buildAexp :: [Token] -> Aexp
buildAexp tokens =
    case parseSumOrHigher tokens of
        (expr, []) -> expr
        (_, _) -> error "Error building arithmetic expression"

parseSumOrHigher :: [Token] -> (Aexp, [Token])
parseSumOrHigher tokens =
    case parseMultOrHigher tokens of
        (expr1, TokenAdd : restTokens1) ->
            case parseSumOrHigher restTokens1 of
                (expr2, restTokens2) -> (AddA expr1 expr2, restTokens2)
        (expr1, TokenSub : restTokens1) ->
            case parseSumOrHigher restTokens1 of
                (expr2, restTokens2) -> (SubA expr2 expr1, restTokens2)
        result -> result

parseMultOrHigher :: [Token] -> (Aexp, [Token])
parseMultOrHigher tokens =
    case parseAtom tokens of
        (expr1, TokenMult : restTokens1) ->
            case parseMultOrHigher restTokens1 of
                (expr2, restTokens2) -> (MultA expr1 expr2, restTokens2)
        result -> result

parseAtom :: [Token] -> (Aexp, [Token])
parseAtom (TokenNum n : restTokens) = (Num n, restTokens)
parseAtom (TokenVar var : restTokens) = (Var var, restTokens)
parseAtom (TokenOpenP : restTokens1) =
    case parseSumOrHigher restTokens1 of
        (expr, TokenCloseP : restTokens2) -> (expr, restTokens2)
        _ -> error "Error parsing atom (vars, consts and parenthesis-wraped expressions)"  -- no closing parenthesis or not parseable expression
parseAtom _ = error "Error parsing atom (vars, consts and parenthesis-wraped expressions)"

-- Parse and build Boolean expressions

buildBexp :: [Token] -> Bexp
buildBexp tokens = case parseAndOrHigher tokens of
    (expr, []) -> expr
    _ -> error "Error building boolean expression"


parseAndOrHigher :: [Token] -> (Bexp, [Token])
parseAndOrHigher tokens = case parseBoolEqOrHigher tokens of
    (expr1, TokenAnd : restTokens1) ->
        case parseAndOrHigher restTokens1 of
            (expr2, restTokens2) -> (AndB expr1 expr2, restTokens2)
    result -> result

parseBoolEqOrHigher :: [Token] -> (Bexp, [Token])
parseBoolEqOrHigher tokens = case parseNotOrHigher tokens of
    (expr1, TokenEqB : restTokens1) ->
        case parseBoolEqOrHigher restTokens1 of
            (expr2, restTokens2) -> (EqB expr1 expr2, restTokens2)
    result -> result

parseNotOrHigher :: [Token] -> (Bexp, [Token])
parseNotOrHigher (TokenNot : rest) = case parseNotOrHigher rest of
    (expr, restTokens) -> (NotB expr, restTokens)
parseNotOrHigher tokens = parseIntEqOrHigher tokens

parseIntEqOrHigher :: [Token] -> (Bexp, [Token])
parseIntEqOrHigher tokens = case parseSumOrHigher tokens of
    (expr1, TokenEqA : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            (expr2, restTokens2) -> (EqA expr1 expr2, restTokens2)
    result -> parseLeOrHigher tokens

parseLeOrHigher :: [Token] -> (Bexp, [Token])
parseLeOrHigher tokens = case parseSumOrHigher tokens of
    (expr1, TokenLe : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            (expr2, restTokens2) -> (LeB expr1 expr2, restTokens2)
    result -> parseTrueParen tokens  -- if cannot parseAexp or there is no LessOrEqTok

parseTrueParen :: [Token] -> (Bexp, [Token])
parseTrueParen (TokenTrue : restTokens) = (TrueB, restTokens)
parseTrueParen (TokenFalse : restTokens) = (TrueB, restTokens)
parseTrueParen (TokenOpenP : restTokens1) = case parseAndOrHigher restTokens1 of
    (expr, TokenCloseP : restTokens2) -> (expr, restTokens2)



type ResultTokens = [Token]
type RemainderTokens = [Token]
type ParenthesisStack = [Char]

-- assumes the expression always has parentheses (must start with OpenParenTok)
getBetweenParenTokens :: [Token] -> (ResultTokens, RemainderTokens)
getBetweenParenTokens tokens = (elseTokens, restTokens)
  where (restTokens, _, elseTokens) = getBetweenParenTokensAux tokens [] []

-- Receives tokens to process, stack and current result
-- Returns remainder, stack, and result
getBetweenParenTokensAux :: RemainderTokens -> ParenthesisStack -> ResultTokens
    -> (RemainderTokens, ParenthesisStack, ResultTokens)
-- reverse the result since tokens are inserted in reversed order
-- no more tokens, return result
getBetweenParenTokensAux [] stk res = ([], [], reverse res)

-- push parenthesis to stack
getBetweenParenTokensAux (TokenOpenP:tokens) stk res = 
    getBetweenParenTokensAux tokens ('(':stk) res

-- pop parenthesis from stack
getBetweenParenTokensAux (TokenCloseP:tokens) stk res = 
    getBetweenParenTokensAux tokens (tail stk) res

-- stack is empty (parentheses fully closed) -> return result
-- if stack non-empty (non-closed parentheses), token is part of the expression
getBetweenParenTokensAux (tok:tokens) stk res    
    | null stk   = (tok:tokens, [], reverse res)
    | otherwise  = getBetweenParenTokensAux tokens stk (tok:res)

-- Parser
parse :: String -> Program
parse = buildData . lexer

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