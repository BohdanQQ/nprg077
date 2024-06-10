// ----------------------------------------------------------------------------
// 06 - Absolute and relative addresses
// ----------------------------------------------------------------------------

// NOTE: Location can be either fixed (absolute) or normal (relative)
// Address is used in 'Reference' and can be either. Raw address is
// actual location in a sheet and this remains just a pair of numbers.
type Location = Fixed of int | Normal of int
type RawAddress = int * int
type Address = Location * Location

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

// NOTE: Sheet is now indexed by raw address
type LiveSheet = Map<RawAddress, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let safeDiv a b = 
  if b = 0 then None else Some(a / b)
let getInt (x:Location) = match x with | Fixed x -> x | Normal x -> x
let getRaw ((locationX: Location, locationY: Location)) = RawAddress(getInt locationX, getInt locationY)

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
        match Map.tryFind (getRaw addr) sheet with
          | Some cellNode -> cellNode.Value
          | None -> Error "Missing value"  


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
  // TODO: Implement this helper which relocates only relative locations.
  // It makes updating relocateReferences easier!
  match loc with | Fixed x -> Fixed x | Normal x -> Normal (x + by)


let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  // TODO: This needs to be updated to only relocate relative references!
  let diffCol = tgtCol - srcCol
  let diffRow = tgtRow - srcRow

  match srcExpr with
    | Const v -> Const v
    | Function (name, args) -> 
        Function (name, List.map (relocateReferences (srcCol, srcRow) (tgtCol, tgtRow)) args)
    | Reference (srcCol, srcRow) -> Reference (relocateLocation srcCol diffCol, relocateLocation srcRow diffRow)


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
  // TODO: Use the original parsing code here for parsing raw addresses
  if s.Length < 2 then failwith "Invalid address"
  let column = int s.[0] - int 'A' + 1
  let row = int s.[1..]
  column, row

let addr (s:string) : Address = 
  // TODO: This is tricky to get right. See the test cases below.
  // You can use regex magic, or have a bunch of nested ifs - 
  // starting with a check if s.[0] = '$' etc. You could also convert
  // string to list using List.ofSeq and use pattern matching.
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



addr "C10" = (Normal 3, Normal 10)
addr "$C10" = (Fixed 3, Normal 10)
addr "C$10" = (Normal 3, Fixed 10)
addr "$C$10" = (Fixed 3, Fixed 10)
addr "C$10"

let continents = 
  [ "Asia", 4753079, 31033; 
    "Africa", 1460481, 29648; 
    "Europe", 740433, 22134; 
    "North America", 604182, 21330; 
    "South America", 439719, 17461; 
    "Australia/Oceania", 46004, 8486; 
    "Antarctica", 0, 13720 ]

let wsheet0 = 
  [ // Column headers
    yield raddr "A1", Const(String "Continent")
    yield raddr "B1", Const(String "Population (thousands)")
    yield raddr "C1", Const(String "Area (thousands km^2)")
    
    // Fill rows of the data table
    for i, (cont, pop, area) in Seq.indexed continents do
      yield raddr $"A{i+2}", Const(String cont)
      yield raddr $"B{i+2}", Const(Number pop)
      yield raddr $"C{i+2}", Const(Number area)
    
    // Add summary row for the world
    yield raddr "A9", Const(String "World")
    yield raddr "B9", Const(Number 8043898) 
    yield raddr "C9", Const(Number 143812)

    // Add relative population
    yield raddr "D1", Const(String "Population (%)")
    yield raddr "D2", Function("/", [ 
        Function("*", [ Reference(addr "B2"); Const(Number 100) ])
        Reference(addr "$B$9")
      ])

    // Add relative area
    yield raddr "E1", Const(String "Area (%)")
    yield raddr "E2", Function("/", [ 
        Function("*", [ Reference(addr "C2"); Const(Number 100) ])
        Reference(addr "$C$9")
      ])

    // Add density of the region
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