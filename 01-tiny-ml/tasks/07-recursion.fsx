// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
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
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  | Recursive of string * Expression * Expression

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
    // failwith "not implemented"
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
    let rec ctx' = Map.add v (lazy(evaluate ctx' e1)) ctx
    evaluate ctx' e2

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 5)
  )
evaluate Map.empty er
