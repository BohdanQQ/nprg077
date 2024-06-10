// ----------------------------------------------------------------------------
// 07 - Adding range selection and array values
// ----------------------------------------------------------------------------

type Location = Fixed of int | Normal of int
type RawAddress = int * int
type Address = Location * Location

type Value = 
  | Number of int
  | String of string
  | Error of string
  // NOTE: Range can be evaluated to a value, which is an array of values
  | Array of Value list

type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list
  // NOTE: Range specifies a region as two corners of a rectangle
  | Range of Address * Address

type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr
    Updated : Event<unit> } 

type LiveSheet = Map<RawAddress, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let safeDiv a b = 
  if b = 0 then None else Some(a / b)
let getInt (x:Location) = match x with | Fixed x -> x | Normal x -> x
let getRaw ((locationX: Location, locationY: Location)) = RawAddress(getInt locationX, getInt locationY)

let enumerateRange upperLeft lowerright =
  let (upX, upY) = getRaw upperLeft
  let (loX, loY) = getRaw lowerright

  List.allPairs [for i in upX .. loX -> i] [for i in upY .. loY -> i] |> List.map RawAddress


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
          | "SUM", [arg] ->
                match eval sheet arg with
                    | Array l -> 
                    let fn = (fun (acc: Value) (v: Value) ->
                          let nVal = match v with | Number x -> x | _ -> failwith "Not a number - SUM - val"
                          match acc with
                            | Error x -> Error x
                            | Number n -> Number (n + nVal)
                            | _ -> failwith "Undefined usage - SUM"
                        )
                    l |> List.fold fn (Number 0)
                    | _ -> failwith "Invalid arguments (SUM)"
          | _ -> Error "Unknown function"
    | Reference addr -> 
        match Map.tryFind (getRaw addr) sheet with
          | Some cellNode -> cellNode.Value
          | None -> Error "Missing value"
    | Range (corner1, corner2) ->
      let addrs = enumerateRange corner1 corner2
      let fn = (fun (acc: Value) (addr: RawAddress) ->  
        match acc with
          | Error x -> Error x
          | Array a -> 
            match Map.tryFind addr sheet with
              | Some cellNode -> Array(cellNode.Value::a)
              | None -> Error "Missing value"
          | _ -> failwith "undefined usage"
      )
      
      addrs |> List.fold fn (Array(List.Empty))

  // TODO: This needs to be modified in two ways.
  //
  // * Handle the 'Range' case. You will need to extract the raw
  //   addresses of the two corners and iterate over all cells in the range.
  //   You can then use 'List.tryMapAll' (from the lecture) to get all the
  //   values. If they are all there, return 'Array', otherwise 'Error'.
  //
  // * Add 'SUM' function that takes one argument which is 'Array'
  //   (defined by range) and sums all elements of the array. This only
  //   works if they are all numerical. Use 'List.tryMapAll' here too!
  //

let rec collectReferences (expr:Expr) : list<RawAddress> = 
  // TODO: Modify the function to return a list of raw addresses!
  List.collect (fun arg -> 
    match arg with
      | Reference addr -> [getRaw addr]
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

let relocateLocation (loc:Location) (by:int) : Location = 
  match loc with | Fixed x -> Fixed x | Normal x -> Normal (x + by)


let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  let diffCol = tgtCol - srcCol
  let diffRow = tgtRow - srcRow

  match srcExpr with
    | Const v -> Const v
    | Function (name, args) -> 
        Function (name, List.map (relocateReferences (srcCol, srcRow) (tgtCol, tgtRow)) args)
    | Reference (srcCol, srcRow) -> Reference (relocateLocation srcCol diffCol, relocateLocation srcRow diffRow)
    | Range (srcCorner, destCorner) -> Range (srcCorner, destCorner)


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
  match v with
    | Number n -> $"<span>{n.ToString()}</span>"
    | String s -> $"<span>{s}</span>"
    | Error e -> $"<span class='e'>{e}</span>"
  
let display (sheet:LiveSheet) = 
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

  wr.Write("<tr><th></th>")
  for col in 1 .. maxCol do 
    wr.Write($"<th>{char (col + int 'A' - 1)}</th>")
  wr.Write("</tr>")

  for row in 1 .. maxRow do 
    wr.Write($"<tr>\n\t<th>{row}</th>\n")
    for col in 1 .. maxCol do
      let str = 
        match Map.tryFind (col, row) sheet with
            | Some(x) ->  displayValue x.Value
            | _ -> ""
      wr.Write($"\t<td>{str}</td>\n")
    wr.Write("</tr>\n")
  wr.Write("</table></body></html>")
  wr.Close()
  Process.Start(f)


// ----------------------------------------------------------------------------
// Helpers and continents demo
// ----------------------------------------------------------------------------

let raddr (s:string) : RawAddress = 
  if s.Length < 2 then failwith "Invalid address"
  let column = int s.[0] - int 'A' + 1
  let row = int s.[1..]
  column, row

let addr (s:string) : Address = 
  if s.Length < 2 then failwith "Invalid address"
  let chars = List.ofSeq s
  // this is bad, im just lazy
  let dollarCount = chars |> List.filter (fun x -> x = '$') |> List.length

  if dollarCount > 2 then failwith "Invalid address"
  let fromRaddr st =
        let (x, y) = raddr st
        Address(Normal x, Normal y)
  if dollarCount = 0 then fromRaddr s
  else
    let pre1 = s.Substring(0, (s.IndexOf '$')) 
    let post1 = s.Substring((s.IndexOf '$') + 1)
    if pre1.Length = 0 then
      if dollarCount = 1 then 
        let (x, y) = raddr post1
        Address(Fixed x, Normal y)
      else 
        let pre2 = post1.Substring(0, (post1.IndexOf '$'))
        let post2 = post1.Substring(post1.IndexOf('$') + 1)
        if pre2.Length = 0 || post2.Length = 0 then failwith "Invalid address"
        else  
          let (x, y) = raddr (pre2 + post2) 
          Address(Fixed x, Fixed y)
    else
      if dollarCount = 1 then 
        let (x, y) = raddr (pre1 + post1)
        Address(Normal x, Fixed y)
      else 
        let pre2 = post1.Substring(0, (post1.IndexOf '$'))
        let post2 = post1.Substring(post1.IndexOf('$') + 1)
        if pre2.Length = 0 || post2.Length = 0 then failwith "Invalid address"
        else 
          let (x, y) = raddr (pre2 + post2) 
          Address(Fixed x, Fixed y)



let continents = 
  [ "Asia", 4753079, 31033; 
    "Africa", 1460481, 29648; 
    "Europe", 740433, 22134; 
    "North America", 604182, 21330; 
    "South America", 439719, 17461; 
    "Australia/Oceania", 46004, 8486; 
    "Antarctica", 0, 13720 ]

let wsheet0 = 
  [ yield raddr "A1", Const(String "Continent")
    yield raddr "B1", Const(String "Population (thousands)")
    yield raddr "C1", Const(String "Area (thousands km^2)")
    for i, (cont, pop, area) in Seq.indexed continents do
      yield raddr $"A{i+2}", Const(String cont)
      yield raddr $"B{i+2}", Const(Number pop)
      yield raddr $"C{i+2}", Const(Number area)
    yield raddr "A9", Const(String "World")
    
    // NOTE: We can now use our new SUM function here!
    yield raddr "B9", Function("SUM", [ Range(addr "B2", addr "B8") ])
    yield raddr "C9", Function("SUM", [ Range(addr "C2", addr "C8") ])

    yield raddr "D1", Const(String "Population (%)")
    yield raddr "D2", Function("/", [ 
        Function("*", [ Reference(addr "B2"); Const(Number 100) ])
        Reference(addr "$B$9")
      ])
    yield raddr "E1", Const(String "Area (%)")
    yield raddr "E2", Function("/", [ 
        Function("*", [ Reference(addr "C2"); Const(Number 100) ])
        Reference(addr "$C$9")
      ])
    yield raddr "F1", Const(String "Density (pop/km^2)")
    yield raddr "F2", Function("/", [ Reference(addr "B2"); Reference(addr "C2") ])
  ]
  |> makeSheet

// Display the initial sheet
display wsheet0

// Now expand all the calculations
let wsheet = 
  wsheet0
  |> expand (raddr "D2") (raddr "D9")
  |> expand (raddr "E2") (raddr "E9")
  |> expand (raddr "F2") (raddr "F9")

// ...and display the resulting sheet!
display wsheet