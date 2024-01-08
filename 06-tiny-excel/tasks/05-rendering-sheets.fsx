// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
// ----------------------------------------------------------------------------

type Address = int * int

type Value = 
  | Number of int
  | String of string
  | Error of string
  
type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr
    Updated : Event<unit> } 

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let safeDiv a b = 
  if b = 0 then None else Some(a / b)

let rec eval (sheet:LiveSheet) expr = 
  match expr with
    | Const v -> v
    | Function (name, args) -> 
        match name, args with
          | "+", [lArg; rArg] ->
              match eval sheet lArg, eval sheet rArg with
                | Number left, Number right -> Number (left + right)
                | _ -> Error "Invalid arguments (+)"
          | "*", [lArg; rArg] -> 
              match eval sheet lArg, eval sheet rArg with
                | Number left, Number right -> Number (left * right)
                | _ -> Error "Invalid arguments (*)"
          | "-" , [lArg; rArg] -> 
              match eval sheet lArg, eval sheet rArg with
                | Number left, Number right -> Number (left - right)
                | _ -> Error "Invalid arguments (-)"
          | "/" , [lArg; rArg] ->
                match eval sheet lArg, eval sheet rArg with
                    | Number left, Number right -> 
                        match safeDiv left right with
                            | Some result -> Number result
                            | None -> Error "Zero division"
                    | _ -> Error "Invalid arguments (/)"
          | _ -> Error "Unknown function"
    | Reference addr -> 
        match Map.tryFind addr sheet with
          | Some cellNode -> cellNode.Value
          | None -> Error "Missing value"  


let rec collectReferences (expr:Expr) : Address list = 
  List.collect (fun arg -> 
    match arg with
      | Reference addr -> [addr]
      | Function (_, args) -> List.map collectReferences args |> List.concat
      | _ -> []) [expr]

let makeNode (sheet:LiveSheet) expr = 
  let value = eval sheet expr
  let result = { Value = value; Expr = expr; Updated = Event<unit>() }
  let updateFn = fun _ -> 
    let newValue = eval sheet expr
    result.Value <- newValue
    result.Updated.Trigger()

  collectReferences expr |> List.iter (fun addr -> 
    match Map.tryFind addr sheet with
      | Some cellNode -> cellNode.Updated.Publish.Add(updateFn)
      | None -> failwith "Cell address invalid")
  result

let updateNode addr (sheet:LiveSheet) expr = 
  let node = Map.find addr sheet
  node.Expr <- expr
  node.Value <- eval sheet expr
  node.Updated.Trigger()

let makeSheet list = 
  List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode sheet expr) sheet) Map.empty list

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  let diffCol = tgtCol - srcCol
  let diffRow = tgtRow - srcRow

  match srcExpr with
    | Const v -> Const v
    | Function (name, args) -> 
        Function (name, List.map (relocateReferences (srcCol, srcRow) (tgtCol, tgtRow)) args)
    | Reference (srcCol, srcRow) -> Reference (srcCol + diffCol, srcRow + diffRow)
let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet:LiveSheet) : LiveSheet = 
  let formulae = [ 
    for col in srcCol .. tgtCol do
      for row in srcRow .. tgtRow do
        yield (col, row), relocateReferences (srcCol, srcRow) (col, row) (Map.find (srcCol, srcRow) sheet).Expr]
  formulae |> List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode sheet expr) sheet) sheet
// ----------------------------------------------------------------------------
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v:Value) : string =
  // TODO: Turn the given value into a string representing HTML
  // You can use the following to create an error string in red.
  match v with
    | Number n -> $"<span>{n.ToString()}</span>"
    | String s -> $"<span>{s}</span>"
    | Error e -> $"<span class='e'>{e}</span>"
  
let display (sheet:LiveSheet) = 
  // TODO: Find the greates row and column index
  let maxCol = List.fold max 0 (Map.keys sheet |> Seq.toList |> List.map (fun (col, _) -> col))
  let maxRow = List.fold max 0 (Map.keys sheet |> Seq.toList |> List.map (fun (_, row) -> row))

  let f = Path.GetTempFileName() + ".html"
  use wr = new StreamWriter(File.OpenWrite(f))
  wr.Write("""<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; } 
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } 
        th { background:#606060; color:white; } 
      </style>
    </head><body><table>""")

  // TODO: Write column headings
  wr.Write("<tr><th></th>")
  for col in 1 .. maxCol do 
    wr.Write($"<th>{char (col + int 'A' - 1)}</th>")
  wr.Write("</tr>")

  // TODO: Write row headings and data
  for row in 1 .. maxRow do 
    wr.Write($"<tr><th>{row}</th>")
    for col in 1 .. maxCol do
      let str = 
        match Map.tryFind (col, row) sheet with
            | Some(x) ->  displayValue x.Value
            | _ -> ""
      wr.Write($"<td>{str}</td>")
    wr.Write("</tr>")
  wr.Write("</table></body></html>")
  wr.Close()
  Process.Start(f)


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  if s.Length < 2 then failwith "Invalid address"
  let column = int s.[0] - int 'A' + 1
  let row = int s.[1..]
  column, row

// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")
display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")
display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4! 
let tempConv = 
  [ addr "A1", Const(String "F to C")
    addr "B1", Const(Number 0) 
    addr "C1", 
      Function("/", [ 
        Function("*", [ 
          Function("-", [ Reference(addr "B1"); Const(Number 32) ])
          Const(Number 5) ])
        Const(Number 9) ]) 
    addr "A2", Const(String "C to F")
    addr "B2", Const(Number 0) 
    addr "C2",  Function("+", [ 
        Function("/", [ 
          Function("*", [ Reference(addr "B2"); Const(Number 9) ])
          Const(Number 5) ])
        Const(Number 32) ]) ]
  |> makeSheet
display tempConv
