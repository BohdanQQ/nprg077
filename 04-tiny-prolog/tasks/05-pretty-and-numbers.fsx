// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with 
  | Atom("zero") -> Some(0)
  | Predicate("succ", [Number n]) -> Some(n+1)
  | _ -> None


let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) -> 
      p + "(" + String.concat " " (items |> List.map formatTerm) + ")"

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
    printfn "%A" ((List.rev subst) |> List.map (fun (s, t) -> s + " = " + formatTerm t ))
// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  if n = 0 then Atom("zero") else Predicate("succ", [num (n - 1)])

// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
