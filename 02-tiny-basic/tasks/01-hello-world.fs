// ----------------------------------------------------------------------------
// 01 - Add GOTO and better PRINT for infinite loop fun!
// ----------------------------------------------------------------------------

// NOTE: You can run this using 'dotnet run' from the terminal. 
// If you want to run code in a different file, you will need to change
// the 'tinybasic.fsproj' file (which references this source file now).

// NOTE: F# code in projects is generally organized using namespaces and modules.
// Here, we declare module name for the source code in this file.
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  // NOTE: GOTO specified line number. Note that this is an integer, rather 
  // than an expression, so you cannot calculate line number dynamically. 
  // (But there are tricks to do this by direct memory access on a real C64!)
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  // TODO: Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
    | StringValue s -> System.Console.Write(s)

let rec getLine state line =
  // TODO: Get a line with a given number from 'state.Program' (this can fail 
  // if the line is not there.) You need this in the 'Goto' command case below.
  match state.Program with
    | [] -> None
    | (l, c) :: rest -> if l = line then Some(l, c) else getLine { Program = rest } line

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr = 
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
      match getLine state line with
        | None -> failwith "GOTO: line not found"
        | Some(l, c) -> runCommand state (l, c)
and runNextLine state line = 
  let rec go originalState state =
    match state.Program.Tail with
        | [] -> originalState
        | (l, c)::[] -> if l > line then runCommand state (l, c) else originalState
        | (l, _)::x::rest -> if l = line then runCommand state x  else go originalState { Program = x::rest }
  go state state


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) ] }

let helloInf = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) 
      20, Goto 10 ] }

// NOTE: First try to get the following to work!
runCommand helloOnce (-1, Run) |> ignore

// NOTE: Then add 'Goto' and get the following to work!
runCommand helloInf (-1, Run) |> ignore

