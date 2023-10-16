// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
  | ValCase of bool * Value

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
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

and VariableContext = 
  Map<string, Value>

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
      // TODO: We added 'ValClosure' to 'Value', so this can now fail to 
      // match (if you call binary operator with functions as arguments).
      // Add a catch-all ('_') case and throw an exception using 'failwith'
      // Also do the same for 'Unary' an 'If'!
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
       | _ -> failwith "operation between the types (bin) not supported"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following two from before
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
            let callCtx = Map.add v argVal closureCtx
            evaluate callCtx e
        | _ -> failwith "operation between the types (appl) not supported"
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
  | Let(v, e1, e2) ->
    // TODO: There are two ways to do this! A nice tricky is to 
    // treat 'let' as a syntactic sugar and transform it to the
    // 'desugared' expression and evaluating that :-)
    // let v = e1 in e2
    let ctx' = Map.add v (evaluate ctx e1) ctx
    evaluate ctx' e2
    // failwith "not implemented"
  | Tuple(e1, e2) ->
      // TODO: Construct a tuple value here!
      ValTuple(evaluate ctx e1, evaluate ctx e2)
    //   failwith "not implemented"
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
            let newCtx = Map.add v caseVal ctx
            if b then
                evaluate newCtx e1
            else
                evaluate newCtx e2
        | _ -> failwith "operation between the types (match) not supported"
    //   // TODO: Implement pattern matching. Note you need to
    //   // assign the right value to the variable of name 'v'!
    //   failwith "not implemented"

  | Case(b, e) -> ValCase(b, evaluate ctx e)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
