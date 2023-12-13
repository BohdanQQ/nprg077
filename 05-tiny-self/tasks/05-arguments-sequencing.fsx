// ----------------------------------------------------------------------------
// 05 - Arguments and sequencing of expressions
// ----------------------------------------------------------------------------

type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
let makeDataObject slots = 
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject slots special = 
  { Code = None; Special = Some special; Slots = slots }

let makeSlot n contents = 
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot n contents = 
  { Name = n; Contents = contents; IsParent = true }

let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

// NOTE: Implemented in step #2
let addSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  let slot = makeSlot n contents
  obj.Slots <- slot :: obj.Slots

let addParentSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  let slot = makeParentSlot n contents
  obj.Slots <- slot :: obj.Slots

let cloneObject (obj:Objekt) : Objekt = 
  let result = makeDataObject obj.Slots
  result.Code <- obj.Code
  result.Special <- obj.Special
  result

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup (msg:string) (obj:Objekt) : list<Objekt * Slot> =  
   match obj.Slots |> List.filter (fun s -> s.Name = msg) with 
    | [] -> parentLookup msg obj
    | s -> s |> List.map (fun x -> (obj, x))
and parentLookup msg obj : list<Objekt * Slot> = obj.Slots |> List.filter (fun s -> s.IsParent ) |> List.map (fun s -> lookup msg s.Contents) |> List.concat

let rec eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue.Code with
    | None -> slotValue
    | Some code -> 
        match code.Special with
          | Some(Native f) -> 
            let clone = cloneObject code
            clone |> addParentSlot "self*" instance
            clone |> addParentSlot "args*" args
            f clone
          | Some(String s) -> printf "%s" s; failwith "what"
          | None -> 
             send "eval" (makeDataObject [ makeSlot "activation" (makeDataObject [ makeParentSlot "self*" instance; makeParentSlot "args*" args ]) ]) code
and send (msg:string) (args:Objekt) (instance:Objekt) : Objekt = 
  match lookup msg instance with
    | [ (_, slot) ] -> eval slot.Contents args instance
    | _ -> failwithf "Message '%s' not supported by the objekt %A!" msg instance

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d in %A)!" n sl.Length sl

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// NOTE: Implemented in step #2
let empty : Objekt = makeDataObject []

let printCode = makeNativeMethod (fun arcd ->
  printf "%s" (getStringValue arcd)
  empty
)

let stringPrototype = makeDataObject [
  makeSlot "print" printCode  
]
let makeString s = 
  makeDataObject [ 
    makeSlot "value" (makeSpecialObject [] (String s))
    makeParentSlot "parent*" stringPrototype 
  ]
// ----------------------------------------------------------------------------
// Cloning and assignments
// ----------------------------------------------------------------------------

let cloneMethod = makeNativeMethod (fun arcd ->  
  let target = lookup "self*" arcd
  match target with
  | [(_, s)] -> makeDataObject (s.Contents.Slots |> List.map (fun s -> {Contents = s.Contents; Name = s.Name; IsParent = s.IsParent }))
  | [] -> failwith "error clone - empty"
  | _ -> failwith "error clone - mult" )
  // (If the lookup returns a wrong thing, fail - that's wrong.)

let clonablePrototype = 
  makeDataObject [
    makeSlot "clone" cloneMethod
  ]

let assignmentMethod n = makeNativeMethod (fun arcd -> 
  let (obj, _) = 
    match lookup n arcd with
      | [x] -> x
      | _ -> failwith "error lookup asgn target"

  let arg = 
    match lookup "new" arcd with
      | [(_, a)] -> a
      | _ -> failwith "error lookup asgn value" 
  obj.Slots <- obj.Slots |> List.map (fun sl -> if sl.Name = n then makeSlot n arg.Contents else sl )
  obj
)

let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }

// ----------------------------------------------------------------------------
// TinySelf code representation & interpreter
// ----------------------------------------------------------------------------

let exprSelf = makeDataObject [
    makeSlot "eval" (makeNativeMethod (fun arcd -> 
       lookupSlotValue "self*" (lookupSlotValue "activation" arcd)
    ))
]

let exprString (s:string) = makeDataObject [ 
  makeSlot "string" (makeString s) 
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    lookupSlotValue "string" arcd
  )) ]

let exprSend msg rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun arcd -> 
    let actr = lookupSlotValue "activation" arcd
    let dObj = makeDataObject [ makeSlot "activation" actr ]
    let msgName = getStringValue (lookupSlotValue "msg" arcd)
    let receiver = lookupSlotValue "receiver" arcd
    let evalueatedRcvr = send "eval" dObj receiver
    send msgName empty evalueatedRcvr 
  )) ]

// TODO: This one is done for you. 'exprSelf' gives you access to the
// object on which a method is called, but if we want to get method
// arguments, those will be stored in the activation record. We get them
// by sending message (with the argument name) to 'exprImplicit'
let exprImplicit = makeDataObject [
  makeSlot "eval" (makeNativeMethod (fun msg ->
    msg |> lookupSlotValue "activation" 
  )) ]


let exprSeq e1 e2 = makeDataObject [ 
  makeSlot "e1" e1
  makeSlot "e2" e2
  makeSlot "eval" (makeNativeMethod (fun msg ->
    // TODO: Construct the activation record to be passed to recursive
    // 'eval' calls (as in 'exprSend'), 
    let actr = lookupSlotValue "activation" msg
    let dObj = makeDataObject [ makeSlot "activation" actr ]
    //recursively evaluate 'e1',
    // ignore the result, then recursively evaluate 'e2' & return the result
    send "eval" dObj (lookupSlotValue "e1" msg) 
    send "eval" dObj (lookupSlotValue "e2" msg)
  )) ]
  
let exprSendWith msg args rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "args" (makeDataObject [ for k, v in args -> makeSlot k v ])
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun msg -> 
    // TODO: This is like 'exprSend' but the method now optionally can 
    // take arguments. Do the same as in 'exprSend' - but before sending,
    // retrieve 'args' and create a new data object that contains the results
    // of recursively evaluating all the argument expressions in 'args'
    let actr = lookupSlotValue "activation" msg
    let dObj = makeDataObject [ makeSlot "activation" actr ]
    let msgName = getStringValue (lookupSlotValue "msg" msg)
    let receiver = lookupSlotValue "receiver" msg
    let argsSlot = lookupSlotValue "args" msg
    let evalueatedRcvr = send "eval" dObj receiver
    // evaluate receiver in the same context as its arguments (dObj reuse)
    let evalArgs: Objekt = makeDataObject (argsSlot.Slots |> List.map (fun x -> ( makeSlot x.Name ( send "eval" dObj x.Contents)) ))
    send msgName evalArgs evalueatedRcvr 
  )) ]
  
// ----------------------------------------------------------------------------
// Tests - Greetings
// ----------------------------------------------------------------------------

let (++) e1 e2 = exprSeq e1 e2

// Object with 'greet' method that prints "Hello" followed by the
// name specified as method argument and then "!!" string. In Self:
//
//   (| greet = ( 'Hello' print. name print. '!!' print ) |)
//
let greeterObj = makeDataObject [ 
  makeSlot "greet" (makeObject [] (
    ( exprString "Hello " |> exprSend "print" ) ++
    ( exprImplicit |> exprSend "name" |> exprSend "print" ) ++
    ( exprString "!!\n" |> exprSend "print" )
  )) 
]

// Send the 'greet' method directly. 
// This tests 'exprImplicit' and 'exprSeq'
greeterObj 
|> send "greet" 
  (makeDataObject [makeSlot "name" (makeString "Prague")])

// Object that has 'greeter' as a slot and a 'main' method that 
// calls the 'greet' method with a string as argument.
//
//   (| greeter = g. main = ( self greeter greet: 'NPRG077' ) |)
//
let mainObj = makeDataObject [
  makeSlot "greeter" greeterObj
  makeSlot "main" (makeObject [] (
    exprSelf 
    |> exprSend "greeter"
    |> exprSendWith "greet" ["name", exprString "NPRG077"]
  ))
]

// Send the 'main' message - this tests 'exprSendWith'
mainObj |> send "main" empty
