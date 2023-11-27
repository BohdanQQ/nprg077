// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term =
  match term with
    | Atom _ -> term
    | Variable v ->
        match subst.TryFind v with
        | Some t -> t
        | None -> term
    | Predicate (p, l) ->
        Predicate (p, substituteTerms subst l)

and substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) =
  List.map (fun (var, term) -> var, substitute newSubst term) subst

and substituteTerms subst (terms:list<Term>) =
  terms |> List.map (substitute subst)



let rec unifyLists l1 l2 : option<list<string * Term>> =
  match l1, l2 with
  | [], [] -> Some []
  | h1::t1, h2::t2 ->
         match unify h1 h2 with
            | Some headSubst -> 
                let t1s = substituteTerms (Map.ofList headSubst) t1
                let t2s = substituteTerms (Map.ofList headSubst) t2
                match unifyLists t1s t2s with
                    | Some t -> Some ((substituteSubst (Map.ofList t) headSubst) @ t)
                    | _ -> None
            | _ -> None
  | _ ->  None

and unify t1 t2 =
  match t1, t2 with
  | Atom a1, Atom a2 ->
    if a1 = a2 then Some [] else None
  | Predicate (p1, l1), Predicate (p2, l2) ->
    if p1 = p2 then unifyLists l1 l2 else None
  | Variable v, t | t, Variable v ->
    Some [v, t]
  | _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
    let mutable n = 0
    fun () -> n <- n + 1; n

let rec freeVariables term = 
    match term with
    | Atom _ -> []
    | Variable v -> [v]
    | Predicate (_, l) -> 
        l |> List.collect freeVariables


let withFreshVariables (clause:Clause) : Clause =
    let varList = List.distinct (freeVariables clause.Head @ List.collect freeVariables clause.Body)
    let subs = varList |> List.map (fun v -> (v, Variable(v + (nextNumber()).ToString())))

    let newHead = substitute (Map.ofList subs) clause.Head
    let newBody = substituteTerms (Map.ofList subs) clause.Body

    {Head = newHead; Body = newBody}


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
    let program = List.map withFreshVariables program

    program |> List.choose (fun clause ->
        match unify clause.Head query with
        | Some subst -> Some (clause, subst)
        | None -> None
    )

let rec solve program subst goals = 
  match goals with 
  | g::goals -> 
      // TODO: We need to solve the goal (term) 'g'. To do so, find all 
      // matching clauses in the 'program' using 'query' 
        let matches = query program g
      // and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
        for clause, newSubst in matches do
            let newGoals = clause.Body @ goals
            let newGoals = substituteTerms (Map.ofList newSubst) newGoals
      // For each possible solution, we need to add the 'clause.Body' to 
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. 
            let subst = substituteSubst (Map.ofList newSubst) subst
      //Then we need to apply the 
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two 
            let finalSubst = subst @ newSubst
      //and call 'solve' recursively with this new substitution
      // to solve the new goals.
            solve program finalSubst newGoals
  | [] -> 
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    printfn "%A" (List.rev subst)

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> George, Y -> William, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]

