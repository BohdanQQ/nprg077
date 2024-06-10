// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
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

let rec (|List|_|) term : option<list<Term>> = 
  // TODO: If the term represents a list, this should return the 
  // elements of the list collected in an ordinary F# list.
  // If the term is 'Atom("empty")' return Some([])
  // If the term is 'Predicate("cons", [h; tl])' where 'tl' is itself
  // a term representing a list 'l', return Some(h::l).
  match term with
    | Atom("empty") -> Some([])
    | Predicate("cons", [h;List l]) -> Some(h::l)
    | _ -> None


let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | List l -> "[" + String.concat ", " (l |> List.map formatTerm) + "]"
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



let rec solve program subst goals : seq<list<string * Term>> = seq {
  // TODO: We want to change this function to return a lazy sequence
  // of all possible substitutions solving the problem. I already 
  // wrapped the code in 'seq { .. }' block for you. Change the rest
  // to recursively call 'solve' using 'yield!' and return new 
  // solutions using 'yield' (replacing the printing code).
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
            yield! solve program finalSubst newGoals
  | [] -> 
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    yield (List.rev subst)
  }


let run program query = 
  let vars = Set.ofSeq (freeVariables query)
  for subst in solve program [] [query] do
    printfn "%A" (subst |> List.filter (fun (s, _) -> vars.Contains s) |> List.map (fun (s, t) -> s + " = " + formatTerm t ))


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n = 
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

// Queries from previous step (now called using 'run')
run nums (Predicate("add", [num 2; num 3; Variable("X")]))
run nums (Predicate("add", [num 2; Variable("X"); num 5]))
run nums (Predicate("add", [num 2; Variable("Y"); Variable("X")]))

// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

let rec makeList l : Term = 
  match l with
  | [] -> Atom("empty")
  | x::ls -> Predicate("cons", [x; makeList ls])

let append = [ 
  fact (Predicate("append", [Atom("empty"); Variable("X"); Variable("X") ]))
  rule (Predicate("append", [
    Predicate("cons", [Variable("X"); Variable("Y") ])
    Variable("Z"); Predicate("cons", [Variable("X"); Variable("W") ])
  ])) [
    Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ])
  ]
]

let l1to4 = makeList [ for i in 1 .. 4 -> num i ]
let l5to9 = makeList [ for i in 5 .. 9 -> num i ]
let l1to9 = makeList [ for i in 1 .. 9 -> num i ]

let permutation =
  append @ [
    fact (Predicate("perm", [ Atom("empty"); Atom("empty") ]))
    rule (Predicate("perm", [ Variable("L"); Predicate("cons", [Variable("H"); Variable("T")]) ])) [
      Predicate("append", [ Variable("V"); Predicate("cons", [Variable("H"); Variable("U")]); Variable("L") ])
      Predicate("append", [ Variable("V"); Variable("U"); Variable("W") ])
      Predicate("perm", [ Variable("W"); Variable("T") ])
    ]
  ]

// DEMO: Generate all permutations of the list [1 .. 4]
run permutation (Predicate("perm", [l1to4; Variable("X")]))


// ----------------------------------------------------------------------------
// Generating magic squares
// ----------------------------------------------------------------------------

// Custom operator and a hlper function for equality & defining variables
let (.=.) a b = Predicate("eq", [a; b])
let var x = Variable(x)

// TinyProlog is too slow! But if we give it the numbers in an order
// that is close to being a magic square (first row is correct), it will 
// manage to generate a magic square sooner or later...
let l = [ 2;7;6; 1;3;4; 5;8;9 ]

let magic = permutation @ nums @ [
  rule (Predicate("add3", [ var "A"; var "B"; var "C"; var "S" ])) [
    Predicate("add", [ var "A"; var "B"; var "T" ])
    Predicate("add", [ var "T"; var "C"; var "S" ])
  ]
  rule (Predicate("magic", [ var "S"; var "X" ])) [
    yield Predicate("perm", [makeList [ for i in l -> num i ]; var "X"])
    yield var "X" .=. makeList [ var "A1"; var "A2"; var "A3"; var "B1"; 
      var "B2"; var "B3"; var "C1"; var "C2"; var "C3" ]    
    for a, b, c in [ 
      ("A1","A2","A3"); ("B1","B2","B3"); ("C1","C2","C3") 
      ("A1","B1","C1"); ("A2","B2","C2"); ("A3","B3","C3")
      ("A1","B2","C3"); ("A3","B2","C1") ] do
      yield Predicate("add3", [var a; var b; var c; var "S"]) 
  ]
]

run magic (Predicate("magic", [num 15; var "X"]))
