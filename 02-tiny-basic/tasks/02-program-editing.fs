// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
    | StringValue s -> System.Console.Write(s)

let rec getLine state line =
  match state.Program with
    | [] -> None
    | (l, c) :: rest -> if l = line then Some(l, c) else getLine { Program = rest } line
let addLine state (line, cmd) = 
  state.Program 
  |> List.filter(fun (l, _) -> l <> line) 
  |> List.append [(line, cmd)] 
  |> List.sortBy(fun (l, _) -> l) 
  |> fun x -> { Program = x }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr = 
  // TODO: Implement evaluation of expressions. The function should take 
  // 'Expression' and return 'Value'. In this step, it is trivial :-)
  match expr with
    | Const v -> v


let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      printValue (evalExpression expr)
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(line) ->
      // TODO: Find the right line of the program using 'getLine' and call 
      // 'runCommand' recursively on the found line to evaluate it.
      match getLine state line with
        | None -> System.Console.Write("stop"); state
        | Some(l, c) -> runCommand state (l, c)
and runNextLine state line = 
  let rec go originalState state =
    // TODO: Find a program line with the number greater than 'line' and evalaute
    // it using 'evalExpression' (if found) or just return 'state' (if not found).
    match state.Program.Tail with
        | [] -> originalState
        | (l, c)::[] -> if l > line then runCommand state (l, c) else originalState
        | (l, _)::x::rest -> if l = line then runCommand state x  else go originalState { Program = x::rest }
  go state state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  // TODO: Simulate what happens when the user enters a line of code in the 
  // interactive terminal. If the 'line' number is 'Some ln', we want to 
  // insert the line into the right location of the program (addLine); if it
  // is 'None', then we want to run it immediately. To make sure that 
  // 'runCommand' does not try to run anything afterwards, you can pass 
  // 'System.Int32.MaxValue' as the line number to it (or you could use -1
  // and handle that case specially in 'runNextLine')
  match line with 
    | Some ln -> addLine state (ln, cmd)
    | None -> runCommand state (System.Int32.MaxValue, cmd)
      

let runInputs state cmds =
  // TODO: Apply all the specified commands to the program state using 'runInput'.
  // This is a one-liner if you use 'List.fold' which has the following type:
  //   ('State -> 'T -> 'State) -> 'State -> list<'T>
  cmds |> List.fold runInput state

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
