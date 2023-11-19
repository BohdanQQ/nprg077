// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  // NOTE: Added three more kinds of expression from TinyML
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  // NOTE: Added type for functions (of single argument)
  | TyFunction of Type * Type

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
 
let rec substType (subst:Map<string, Type>) ty  = 
  // Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  let rec substTypeImpl ty v = 
    match ty with
      | TyVariable v1 -> if v = v1 then subst.[v] else ty
      | TyBool | TyNumber -> ty
      | TyList t -> TyList (substTypeImpl t v)
      | TyFunction(ret, arg) -> TyFunction(substTypeImpl ret v, substTypeImpl arg v)
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
   | _ -> failwith "Cannot be solved"



// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
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

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a 
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] -> 
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.


// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10))
|> infer 

// let f = fun x -> x*2 in (f 20) + (f 1)
Let("f",
  Lambda("x", Binary("*", Variable("x"), Constant(2))),
  Binary("+", 
    Application(Variable("f"), Constant(20)),
    Application(Variable("f"), Constant(1)) 
  ))
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", 
  Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

// fun f -> f f 
// This does not type check due to occurs check
Lambda("f", 
  Application(Variable "f", Variable "f"))
|> infer

// fun f -> f 1 + f (2 = 3) 
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda("f", 
  Binary("+",
    Application(Variable "f", Constant 1),
    Application(Variable "f", Binary("=", Constant 2, Constant 3))
  ))
|> infer
