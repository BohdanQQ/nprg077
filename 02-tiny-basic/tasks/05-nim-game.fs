// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

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
                let n = 
                    match exprs with 
                        | [Const(NumberValue n)] -> n
                        | _ -> failwith "Invalid arguments for RND"
                NumberValue(state.Random.Next(n))
            | "MIN" -> 
                let e1, e2 = check2Args exprs "MIN"
                match eval e1, eval e2 with
                | NumberValue n1, NumberValue n2 -> NumberValue(System.Math.Min(n1, n2))
                | _ -> failwith "Invalid arguments for MIN"
            | _ -> failwith ("Invalid function " + str) 
    | Variable varName -> match state.Variables.TryFind(varName) with
                            | Some v -> v
                            | None -> failwith ("Variable not found " + varName + state.Variables.ToString())

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(exprs) ->
      exprs |> List.map (fun expr -> evalExpression expr state) |> List.iter printValue
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
  | Clear -> System.Console.Clear(); runNextLine state line
  | Poke (x, y, what) -> 
        let (xVal, yVal, value) = (evalExpression x state, evalExpression y state, evalExpression what state)
        match (xVal, yVal, value) with 
            | NumberValue x, NumberValue y, StringValue what -> System.Console.SetCursorPosition(x, y); System.Console.Write(what);
            | _ -> failwith "Invalid arguments for POKE"
        runNextLine state line
  // TODO: Input("X") should read a number from the console using Console.RadLine
  // and parse it as a number using Int32.TryParse (retry if the input is wrong)
  // Stop terminates the execution (you can just return the 'state'.)
  | Input varName -> 
        let num = System.Console.ReadLine() |> System.Int32.TryParse
        match num with
            | (true, n) -> runNextLine { state with Variables = state.Variables.Add(varName, NumberValue(n)) } line
            | _ -> runCommand state (line, cmd)
  | Stop -> state

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
