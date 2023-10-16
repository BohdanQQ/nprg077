// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value
  | ValUnit 

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  | Recursive of string * Expression * Expression
  | Unit 

and VariableContext = 
  Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | "%" -> ValNum(n1 % n2)
          | _ -> failwith "unsupported binary operator"
       | _ -> failwith "operation between the types (bin) not supported"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res.Value
      | _ -> failwith ("unbound variable: " + v)
  | Unary(op, e) ->
      match op with
      | "-" -> 
          match evaluate ctx e with
            | ValNum n -> ValNum(-n)
            | _ -> failwith "operation between the types (unop) not supported"
      | _ -> failwith "unsupported unary operator"
    | If(condExpr, trueExpr, falseExpr) ->
        let r = evaluate ctx condExpr
        match r with
          | ValNum n -> 
              if n = 1 then
                evaluate ctx trueExpr
              else
                evaluate ctx falseExpr  
          | _ -> failwith "operation between the types (if) not supported"
  | Lambda(v, e) -> ValClosure(v, e, ctx)
  | Application(body, arg) ->
      match evaluate ctx body with
        | ValClosure(v, e, closureCtx) ->
            let argVal = evaluate ctx arg
            let callCtx = Map.add v (lazy(argVal)) closureCtx
            evaluate callCtx e
        | _ -> failwith "operation between the types (appl) not supported"
  | Let(v, e1, e2) ->
    let ctx2 = Map.add v (lazy(evaluate ctx e1)) ctx
    evaluate ctx2 e2
  | Tuple(e1, e2) ->
      ValTuple(evaluate ctx e1, evaluate ctx e2)
  | TupleGet(b, e) ->
      match evaluate ctx e with
        | ValTuple(v1, v2) ->
            if b then
                v1
            else
                v2
        | _ -> failwith "operation between the types (tupleget) not supported"

  | Match(e, v, e1, e2) ->
      match evaluate ctx e with
        | ValCase(b,caseVal) -> 
            let newCtx = Map.add v (lazy(caseVal)) ctx
            if b then
                evaluate newCtx e1
            else
                evaluate newCtx e2
        | _ -> failwith "operation between the types (match) not supported"
  | Case(b, e) -> ValCase(b, evaluate ctx e)

  | Recursive(v, e1, e2) ->
    let rec ctx2 = Map.add v (lazy(evaluate ctx2 e1)) ctx
    evaluate ctx2 e2
  | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2), 
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we 
// do not need to write them by hand!
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1 .. 5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> Case1(f x#1, (map f) x#2) 
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em = 
  Recursive("map",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")),
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x"))
        )),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "map", 
      Lambda("y", Binary("*", Variable "y", Constant 10))), el)
  )
evaluate Map.empty em

// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> 
//          if f x#1 then Case1(x#1, (map f) x#2) 
//          else (map f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y + (-2)) l
// 

let ef =  Recursive("filter",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        If(
            Application(Variable "f", TupleGet(true, Variable "x")),
            Case(true, Tuple(TupleGet(true, Variable "x"), Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x")))),
            Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
        ),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "filter", 
      Lambda("y", Binary("%", Variable "y", Constant(2)))), el)
  )
evaluate Map.empty ef
