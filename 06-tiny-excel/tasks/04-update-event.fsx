// ----------------------------------------------------------------------------
// 04 - Reactive event-based computation
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
    // NOTE: Added event that will be triggered when the 
    // expression and value of the node is changed.
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
  // TODO: Collect the addresses of all references that appear in the 
  // expression 'expr'. This needs to call itself recursively for all
  // arguments of 'Function' and concatenate the returned lists.
  // HINT: This looks nice if you use 'List.collect'.

  List.collect (fun arg -> 
    match arg with
      | Reference addr -> [addr]
      | Function (_, args) -> List.map collectReferences args |> List.concat
      | _ -> []) [expr]


let makeNode (sheet:LiveSheet) expr = 
  // TODO: Add handling of 'Update' events!
  //
  // * When creating a node, we need to create a new event and 
  //   set it as the 'Updated' event of the returned node.
  // * We then need to define 'update' function that will be triggered
  //   when any of the cells on which this one depends change. In the 
  //   function, re-evaluate the formula, set the new value and trigger
  //   our Updated event to notify other cells.
  // * Before returning, use 'collectReferences' to find all cells on which
  //   this one depends and add 'update' as the handler of their 
  //   'Updated' event
  //

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
  // TODO: For now, we ignore the fact that the new expression may have
  // different set of references than the one we are replacing. 
  // So, we can just get the node, set the new expression and value
  // and trigger the Updated event!

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
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  if s.Length < 2 then failwith "Invalid address"
  let column = int s.[0] - int 'A' + 1
  let row = int s.[1..]
  column, row

// Simple spreadsheet that performs conversion between Celsius and Fahrenheit
// To convert F to C, we put value in F into B1 and read the result in C1
// To convert C to F, we put value in C into B2 and read the result in C2
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
    // TODO: Add formula for Celsius to Fahrenheit conversion to 'C2'
    addr "C2",  Function("+", [ 
        Function("/", [ 
          Function("*", [ Reference(addr "B2"); Const(Number 9) ])
          Const(Number 5) ])
        Const(Number 32) ])
  ] 
  |> makeSheet

// Fahrenheit to Celsius conversions

// Should return: -17
updateNode (addr "B1") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C1"))
// Should return: 0
updateNode (addr "B1") tempConv (Const(Number 32))
eval tempConv (Reference(addr "C1"))
// Should return: 37
updateNode (addr "B1") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C1"))

// Celsius to Fahrenheit conversions

// Should return: 32
updateNode (addr "B2") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C2"))
// Should return: 212
updateNode (addr "B2") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C2"))
// Should return: 100
updateNode (addr "B2") tempConv (Const(Number 38))
eval tempConv (Reference(addr "C2"))

