// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

// NOTE: Start with some basic expressions from TinyML
// This time, If requires a real Boolean argument and we have
// operators '+' (int -> int -> int) and '=' (int -> int -> bool)
type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
  // return true if type 'ty' contains variable 'vcheck'
  match ty with
    | TyVariable v -> v = vcheck
    | TyBool | TyNumber -> false
    | TyList t -> occursCheck vcheck t
 
let rec substType (subst:Map<string, Type>) ty  = 
  // Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  let rec substTypeImpl ty v = 
    match ty with
      | TyVariable v1 -> if v = v1 then subst.[v] else ty
      | TyBool | TyNumber -> ty
      | TyList t -> TyList (substTypeImpl t v)
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
   | _ -> failwith "Cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) ->
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
      // TODO: Call generate recursively on all three sub-expressions,
      // collect all constraints and add a constraint that (i) the type
      // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
      let t1, s1 = generate ctx econd
      let t2, s2 = generate ctx etrue
      let t3, s3 = generate ctx efalse
      TyBool, s1 @ s2 @ s3 @ [ t1, TyBool; t2, t3 ]


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = 
  Binary("=",   
    Variable("x"), 
    Binary("+", Constant(10), Variable("x")))

let t1, cs1 = 
  generate (Map.ofList ["x", TyVariable "a"]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("y"))

let t2, cs2 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("x"))

let t3, cs3 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e3

solve cs3
