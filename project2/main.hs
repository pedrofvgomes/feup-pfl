import Data.List (sort, intercalate)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]
type Stack = [Integer]
type Storage = [(String, Integer)]
type State = (Stack, Storage)

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate ", " (map showConverted stack)
  where
    showConverted :: Integer -> String
    showConverted x
      | x == 111 = "True"
      | x == 999 = "False"
      | otherwise = show x

createEmptyState :: State
createEmptyState = ([], [])

state2Str :: State -> String
state2Str state = intercalate ", " [var ++ "=" ++ showWithBoolean val | (var, val) <- sort state]
  where
    showWithBoolean x = case x of
      111 -> "True"
      999 -> "False"
      _ -> show x

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:insts, stack, state) = case inst of
  Push n -> run (insts, n:stack, state)
  Add -> applyBinaryOperation (+)
  Mult -> applyBinaryOperation (*)
  Sub -> applyBinaryOperation (-)
  Tru -> pushValue 111
  Fals -> pushValue 999
  Equ -> applyComparisonOperation (==)
  Le -> applyComparisonOperation (<=)
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
      x:y:rest -> run (insts, (op y x):rest, state)
      _ -> error "Run-time error: Operation requires at least two elements on the stack"

    applyComparisonOperation op = case stack of
      x:y:rest -> if x `op` y then run (insts, 111:rest, state) else run (insts, 999:rest, state)
      _ -> error "Run-time error: Comparison operation requires at least two elements on the stack"

    applyLogicOperation op = case stack of
      x:y:rest -> if x == 111 && y == 111 then run (insts, 111:rest, state) else run (insts, 999:rest, state)
      _ -> error "Run-time error: Logic operation requires at least two elements on the stack"

    applyNegationOperation = case stack of
      x:rest -> run (insts, if x == 111 then 999:rest else 111:rest, state)
      _ -> error "Run-time error: Negation operation requires at least one element on the stack"

    runFetch var = case lookup var (snd state) of
      Just val -> run (insts, val:stack, state)
      Nothing -> error $ "Run-time error: Variable " ++ var ++ " not found in storage"

    runStore var = case stack of
      val:rest -> run (insts, rest, (var, val):(snd state))
      _ -> error "Run-time error: Store operation requires at least one element on the stack"

    runBranch c1 c2 = case stack of
      111:rest -> run (c1, rest, state)
      999:rest -> run (c2, rest, state)
      _ -> error "Run-time error: Branch expects True (111) or False (999) on the stack"

    runLoop c1 c2 = case stack of
      111:rest -> run (c1 ++ [Branch c2 [Loop c1 c2]], rest, state)
      999:rest -> run (Noop:insts, rest, state)
      _ -> error "Run-time error: Loop expects True (111) or False (999) on the stack"

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (code, createEmptyStack, createEmptyState)


main :: IO ()
main = do
  let code1 = [Push 5, Push 3, Add]
      result1 = testAssembler code1
  putStrLn $ show result1

  let code2 = [Tru, Tru, Or]
      result2 = testAssembler code2
  putStrLn $ show result2

  let code3 = [Fals, Fals, Not]
      result3 = testAssembler code3
  putStrLn $ show result3

  -- Simulating a run-time error
  let code4 = [Push 10, Push 5, Le, Fetch "invalidVar"]
      result4 = testAssembler code4
  putStrLn $ show result4

  let code5 = [Push 50, Push 100, Le]
      result5 = testAssembler code5
  putStrLn $ show result5

  let code6 = [Push 4, Push 4, Equ]
      result6 = testAssembler code6
  putStrLn $ show result6

  let code7 = [Push 7, Push 8, Equ]
      result7 = testAssembler code7
  putStrLn $ show result7

  let code8 = [Push 7, Push 8, Equ, Neg]
      result8 = testAssembler code8
  putStrLn $ show result8

  let code9 = [Push 7, Push 8, Equ, Neg, Neg]
      result9 = testAssembler code9
  putStrLn $ show result9

  let code10 = [Push 7, Push 7, Equ, Equ]
      result10 = testAssembler code10
  putStrLn $ show result10







