// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Rng : System.Random
    // TODO: You will need to include random number generator in the state!
    }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
    | StringValue s -> System.Console.Write(s)
    | NumberValue n ->  System.Console.Write(n)
    | BoolValue b -> System.Console.Write(b)

let rec getLine state line =
  match state.Program with
    | [] -> None
    | (l, c) :: rest -> if l = line then Some(l, c) else getLine { state with Program = rest } line
let addLine state (line, cmd) = 
  state.Program 
  |> List.filter(fun (l, _) -> l <> line) 
  |> List.append [(line, cmd)] 
  |> List.sortBy(fun (l, _) -> l) 
  |> fun x -> { state with Program = x }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args name = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith ("Invalid arguments for " + name.ToString())

let rec evalExpression expr state = 
  // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer.
  let eval x = evalExpression x state
  let check2Args x name = 
    match x with
      | e1::e2::_ -> e1, e2
      | _ -> failwith ("Invalid arguments for " + name.ToString())
  match expr with
    | Const v -> v
    | Function(str, exprs) -> 
        match str with
            | "-" -> 
                let e1, e2 = check2Args exprs "-"
                match eval e1, eval e2 with
                        | NumberValue n1, NumberValue n2 -> NumberValue(n1 - n2)
                        | _ -> failwith "Invalid arguments for -"
                
            | "=" -> 
                let e1, e2 = check2Args exprs "="
                match eval e1, eval e2 with
                | NumberValue n1, NumberValue n2 -> BoolValue(n1 = n2)
                | StringValue s1, StringValue s2 -> BoolValue(s1 = s2)
                | BoolValue b1, BoolValue b2 -> BoolValue(b1 = b2)
                | _ -> failwith "Invalid arguments for ="
            | ">" ->
                let e1, e2 = check2Args exprs ">"
                binaryRelOp (fun a b -> a > b) [eval e1; eval e2] ">"
            | "<" -> 
                let e1, e2 = check2Args exprs "<"
                binaryRelOp (fun a b -> a < b) [eval e1; eval e2] "<"
            | "||" -> 
                let e1, e2 = check2Args exprs "||"
                match eval e1, eval e2 with
                | BoolValue b1, BoolValue b2 -> BoolValue(b1 || b2)
                | _ -> failwith "Invalid arguments for ||"
            | "RND" -> 
                // System.Console.WriteLine("RND")
                let n = 
                    match exprs with 
                        | [Const(NumberValue n)] -> n
                        | _ -> failwith "Invalid arguments for RND"
                NumberValue(state.Rng.Next(n))
            | _ -> failwith "Invalid function"
    | Variable varName -> match state.Variables.TryFind(varName) with
                            | Some v -> v
                            | None -> failwith ("Variable not found " + varName + state.Variables.ToString())

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      printValue (evalExpression expr state)
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(line) ->
      match getLine state line with
        | None -> failwith "GOTO: line not found"
        | Some(l, c) -> runCommand state (l, c)
  | Assign (varName, expr) -> 
      let value = evalExpression expr state
      runNextLine { state with Variables = state.Variables.Add(varName, value) } line 
  | If (expr, command) -> 
      match evalExpression expr state with
        | BoolValue true -> runCommand state (line, command)
        | BoolValue false -> runNextLine state line
        | _ -> failwith "Invalid expression in IF"  
  // TODO: Implement two commands for screen manipulation
  | Clear -> System.Console.Clear(); runNextLine state line
  | Poke (x, y, what) -> 
        let (xVal, yVal, value) = (evalExpression x state, evalExpression y state, evalExpression what state)
        match (xVal, yVal, value) with 
            | NumberValue x, NumberValue y, StringValue what -> System.Console.SetCursorPosition(x, y); System.Console.Write(what);
            | _ -> failwith "Invalid arguments for POKE"
        runNextLine state line
and runNextLine state line = 
  let rec go originalState state =
    match state.Program with
        | [] -> originalState
        | (l, c)::[] -> if l > line then runCommand originalState (l, c) else originalState
        | (l, c)::rest -> if l > line then runCommand originalState (l, c)  else go originalState { state with Program = rest }
  go state state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with 
    | Some ln -> addLine state (ln, cmd)
    | None -> runCommand state (System.Int32.MaxValue, cmd)
      

let runInputs state cmds =
  cmds |> List.fold runInput state

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Rng = System.Random.Shared } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
