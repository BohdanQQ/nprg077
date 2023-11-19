// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
  // return true if type 'ty' contains variable 'vcheck'
  match ty with
    | TyVariable v -> v = vcheck
    | TyBool | TyNumber -> false
    | TyList t -> occursCheck vcheck t
    | TyFunction(retval, args) -> occursCheck vcheck retval || occursCheck vcheck args
    | TyTuple(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2
 
let rec substType (subst:Map<string, Type>) ty  = 
  // Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  let rec substTypeImpl ty v = 
    match ty with
      | TyVariable v1 -> if v = v1 then subst.[v] else ty
      | TyBool | TyNumber -> ty
      | TyList t -> TyList (substTypeImpl t v)
      | TyFunction(ret, arg) -> TyFunction(substTypeImpl ret v, substTypeImpl arg v)
      | TyTuple(t1, t2) -> TyTuple(substTypeImpl t1 v, substTypeImpl t2 v)
  and f = (fun acc key vl -> substTypeImpl acc key)
    in
  Map.fold f ty subst


let rec substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // Apply substitution 'subst' to all types in constraints 'cs'
  cs |> List.map (fun (ty1, ty2) -> (substType subst ty1, substType subst ty2))

let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs | (TyBool, TyBool)::cs -> 
      solve cs
  | (TyList t1, TyList t2)::cs -> 
      solve ((t1, t2)::cs)
  | (TyVariable v, t)::cs | (t, TyVariable v)::cs ->
      if occursCheck v t then failwith "Cannot be solved (occurs check)"
      let cs = substConstrs (Map.empty.Add(v, t)) cs
      let subst = solve cs
      let t = substType (Map.ofList subst) t
      (v, t)::subst
   | (TyFunction(ret, arg), TyFunction(ret2, arg2))::cs -> solve ((ret, ret2)::(arg, arg2)::cs)
   | (TyTuple(t1, t2), TyTuple(t12, t22))::cs -> solve ((t1, t12)::(t2, t22)::cs)
   | _ -> failwith "Cannot be solved"

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) |  Binary("*", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ] 
  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      match ctx.TryFind v with 
        | Some t -> t, []
        | None -> failwithf "Variable '%s' not found." v

  | If(econd, etrue, efalse) ->
      let t1, s1 = generate ctx econd
      let t2, s2 = generate ctx etrue
      let t3, s3 = generate ctx efalse
      TyBool, s1 @ s2 @ s3 @ [ t1, TyBool; t2, t3 ]
  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate (ctx.Add(v, t1)) e2
      t2, s1 @ s2
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let ctx = ctx.Add(v, targ)
      let t, s = generate ctx e
      TyFunction(t, targ), s

  | Application(e1, e2) -> 
      let t1, s1 = generate ctx e1
      let tret = newTyVariable()
      let targ = newTyVariable()
      let t2, s2 = generate ctx e2
      tret, s1 @ s2 @ [t1, TyFunction(tret, targ); t2, targ]

  | Tuple(e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1@s2

  | TupleGet(b, e) ->
      let t, s = generate ctx e
      let tfst = newTyVariable()
      let tsnd = newTyVariable()
      (if b then tfst else tsnd), s@[t, TyTuple(tfst, tsnd)]

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
