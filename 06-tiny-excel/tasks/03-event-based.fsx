// ----------------------------------------------------------------------------
// 03 - Reactive event-based structure
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

// Node in a dependency graph that represents a spreadsheet cell
// For each cell, we store the original expression, evalauted value
// and an event to be triggered when the value changes.
type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr } 

// A live spreadsheet is a mapping from addresses to graph nodes
type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr = 
  // TODO: Modify the 'Reference' case. Instead of recursively calling 
  // 'eval', this should now locate the graph node and return the 'Value'
  // that is stored in the graph node!
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
          | _ -> Error "Unknown function"
    | Reference addr -> 
        match Map.tryFind addr sheet with
          | Some cellNode -> cellNode.Value
          | None -> Error "Missing value"

let makeNode (sheet:LiveSheet) (expr:Expr) : CellNode = 
  // TODO: Create a dependency graph node. In this step, we just want
  // to get the same functionality as before (i.e., no event handling)
  // so evaluate the expression, store it and return the node.
  let value = eval sheet expr
  { Value = value; Expr = expr } 


let makeSheet (list:(Address * Expr) list) : LiveSheet = 
  // TODO: Previously, we could turn a list of mappings into a sheet just
  // by using Map.ofList. This no longer works, because we need to add
  // cells one by one (we should make sure that all cells on which the new one
  // depends are already in the sheet, but we assume examples are given
  // in a correct order). To do this, use 'List.fold' and 'makeNode'. 
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
  // TODO: This needs to call 'makeNode' and add the resulting node, 
  // instead of just adding the expression to the map as is.
  let formulae = [ 
    for col in srcCol .. tgtCol do
      for row in srcRow .. tgtRow do
        yield (col, row), relocateReferences (srcCol, srcRow) (col, row) (Map.find (srcCol, srcRow) sheet).Expr]
  formulae |> List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode sheet expr) sheet) sheet

// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  if s.Length < 2 then failwith "Invalid address"
  let column = int s.[0] - int 'A' + 1
  let row = int s.[1..]
  column, row


let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8"))
// Should return: Number 21
eval fib (Reference(addr "A9"))
// Should return: Number 34
eval fib (Reference(addr "A10"))
// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))


let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")

// Should return: Number 5
eval fac (Reference(addr "A6"))
// Should return: Number 120
eval fac (Reference(addr "B6"))

// Should return: Number 10
eval fac (Reference(addr "A11"))
// Should return: Number 3628800
eval fac (Reference(addr "B11"))
