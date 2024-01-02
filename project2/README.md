<div align='center'>
<img src='https://raw.githubusercontent.com/pedrofvgomes/feup-ipc/main/img/feup.png'>

# Functional and Logic Programming
## Haskell Project

## Group members and their respective contributions
- Gaspar Manuel Ferrás Faria (up202108797) - 40%
- Pedro Filipe Vale Gomes (up202108825) - 60%

## Description of the project
This project was divided into two parts. In the first part, we developed a program to manage a stack, executing a sequence of specific operations. In the second part, the aim was to translate a set of commands from a simple programming language into a format understandable by our compiler.

### First part
In the first part, we implemented a stack-based interpreter in Haskell, which includes definitions for instructions (data type Inst), an assembler (testAssembler function), and outlines the structure for managing stacks and states.
The main tasks involve:

- Defining Stack and State:

```haskell
createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []
``````

- Conversion Functions are responsible for converting stacks and states into strings:

```haskell
stack2Str :: Stack -> String
stack2Str stack = intercalate "," stack

state2Str :: State -> String
state2Str = intercalate "," . map (\(var, val) -> var ++ "=" ++ val) . sort
``````

- Interpreter Function is the central function of the interpreter that executes a sequence of instructions on the provided stack and state:

```haskell
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
``````

- The testAssembler function allows testing the assembler with various sets of instructions and checking the resulting stack and state:

```haskell
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
``````
For example, to test the assembler function with a set of instructions performing simple arithmetic operations, we could use the following test:

```haskell
testAssembler [Push 10, Push 4, Push 3, Sub, Mult]
``````

This test aims to perform arithmetic operations by subtracting 3 from 4 and then multiplying the result by 10. The expected output should be ("-10",""), representing the final stack and state after executing these instructions.


### Second part

In this part, we began by defining the compiler that would translate from the language into the list of instructions we had in part one. The initial step was to define three new data types: Aexp for arithmetic expressions, Bexp for boolean expressions, and Stm for statements.

Types Definition: The main objective is to define four crucial types:

- Aexp: Represents arithmetic expressions within the language.

```haskell
data Aexp = 
  Var String | Num Integer | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp
  deriving (Show, Eq)
``````

- Bexp: Stands for boolean expressions used in conditional statements.

```haskell
data Bexp = 
  TrueB | FalseB | AndB Bexp Bexp | EqA Aexp Aexp | EqB Bexp Bexp | LeB Aexp Aexp | NotB Bexp
  deriving (Show, Eq)
``````

- Stm: Denotes statements within the imperative language.

```haskell
data Stm = Assign String Aexp | If Bexp [Stm] [Stm] | While Bexp [Stm] | Seq [Stm]
  deriving (Show, Eq)
``````


Program: Represents the entire program structure.

- Functions to Implement:

     - compA: Responsible for translating arithmetic expressions (Aexp) into a corresponding code.

     ```haskell
     compA :: Aexp -> Code
     compA exp = case exp of
    Var x -> [Fetch x]
    Num n -> [Push n]
    AddA a1 a2 -> compA a2 ++ compA a1 ++ [Add]
    SubA a1 a2 -> compA a2 ++ compA a1 ++ [Sub]
    MultA a1 a2 -> compA a2 ++ compA a1 ++ [Mult]
     ``````

    - compB: Handles the translation of boolean expressions (Bexp) into executable code.

     ```haskell
     compB :: Bexp -> Code
    compB exp = case exp of
    TrueB -> [Tru]
    FalseB -> [Fals]
    AndB b1 b2 -> compB b2 ++ compB b1 ++ [And]
    EqA a1 a2 -> compA a2 ++ compA a1 ++ [Equ]
    EqB b1 b2 -> compB b2 ++ compB b1 ++ [Equ]
    LeB a1 a2 -> compA a2 ++ compA a1 ++ [Le]
    NotB b -> compB b ++ [Neg] 
     ``````

    - compile: Aims to convert an entire program (Program) into executable code.

    ```haskell
    compile :: Program -> Code
    compile [] = []
    compile (Assign var a:rest) = compA a ++ [Store var] ++ compile rest
    compile (If b s1 s2:rest) = compB b ++ [Branch (compile s1) (compile s2)] ++ compile rest
    compile (While b s:rest) = Loop (compB b) (compile s) : compile rest
     `````` 

    - parse: Focuses on parsing a string representation of a program into a structured program understandable by the interpreter.

- The testParser function assists in testing the parser's functionality. It converts a string representation of a program into executable code, executes it, and provides the resulting stack and state.


## Conclusion


This project allowed the integration between the theoretical knowledge acquired in the course and its practical application. During the implementation of the compiler and interpreter, we learned the importance of meticulous error analysis. This highlighted the relevance of careful debugging and a deep understanding of execution flows to ensure the accuracy and reliability of the final program.
