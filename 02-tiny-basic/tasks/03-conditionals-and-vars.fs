// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
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

type State = 
  { 
    Program : list<int * Command> 
    Variables: Map<string, Value>
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

let rec evalExpression expr state = 
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

let empty = { Variables = Map.empty ; Program = [] } // TODO: Add empty variables to the initial state!

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

runInputs empty helloOnce |> ignore
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

System.Console.WriteLine("")
// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
