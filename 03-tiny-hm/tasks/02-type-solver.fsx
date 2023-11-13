// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

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
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
