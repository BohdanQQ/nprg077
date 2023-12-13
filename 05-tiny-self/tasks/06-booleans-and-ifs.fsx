// ----------------------------------------------------------------------------
// 06 - Booleans and 'if' as a message send!
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

let exprImplicit = makeDataObject [
  makeSlot "eval" (makeNativeMethod (fun msg ->
    msg |> lookupSlotValue "activation" 
  )) ]


let exprSeq e1 e2 = makeDataObject [ 
  makeSlot "e1" e1
  makeSlot "e2" e2
  makeSlot "eval" (makeNativeMethod (fun msg ->
    let actr = lookupSlotValue "activation" msg
    let dObj = makeDataObject [ makeSlot "activation" actr ]
    send "eval" dObj (lookupSlotValue "e1" msg) 
    send "eval" dObj (lookupSlotValue "e2" msg)
  )) ]
  
let exprSendWith msg args rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "args" (makeDataObject [ for k, v in args -> makeSlot k v ])
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun msg -> 
    let actr = lookupSlotValue "activation" msg
    let dObj = makeDataObject [ makeSlot "activation" actr ]
    let msgName = getStringValue (lookupSlotValue "msg" msg)
    let receiver = lookupSlotValue "receiver" msg
    let evalueatedRcvr = send "eval" dObj receiver
    let argsSlot = lookupSlotValue "args" msg
    let evalArgs: Objekt = makeDataObject (argsSlot.Slots |> List.map (fun x -> ( makeSlot x.Name ( send "eval" dObj x.Contents)) ))
    send msgName evalArgs evalueatedRcvr 
  )) ]
  
let exprNew slots = makeDataObject [
  makeSlot "slots" (makeDataObject [ for k, v in slots -> makeSlot k v ])
  makeSlot "eval" (makeNativeMethod (fun actr ->
    // TODO: Expression that represents the creation of a new data object.
    // To evaluate this, retrieve & evaluate all expressions in 'slots'
    // (similar to evaluation of arguments in 'exprSendWith') and 
    // construct & return new data object
    let slotsSl = lookupSlotValue "slots" actr
    makeDataObject (slotsSl.Slots |> List.map (fun x -> ( makeSlot x.Name ( send "eval" empty x.Contents)) ))
  )) ]

let exprBlock ebody = makeDataObject [
  makeSlot "body" ebody
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    // NOTE: This is partly done for you - it is impossibly fiddly to debug! When a 
    // block is created, we store its body and get the call-site activation record.
    let body = lookupSlotValue "body" arcd
    let actVal = lookupSlotValue "activation" arcd
    makeDataObject [
      makeSlot "body" body
      makeSlot "value" (makeNativeMethod(fun actv ->
        // The activation record for the body contains 'self*' from the
        // declaration site, and can access 'args' from both the declaration
        // site (static scope) and the args passed to the method (dynamic scope)
        let actVal = actVal |> cloneObject
        let self = actVal |> lookupSlotValue "self*"
        let args1 = actVal |> lookupSlotValue "args*" 
        let args2 = actv |> lookupSlotValue "args*" 
        
        // TODO: Last bit - create data object with 3 parent slots 
        // (args1*, args2* and self*) storing the above 3 objects,
        let dObj = makeDataObject [
            makeParentSlot "args1*" args1
            makeParentSlot "args2*" args2
            makeParentSlot "self*" self
        ]
        // fetch the 'body' expression and send it an 'eval' message.
        // To call 'eval' pass it the newly created object as 'activation'
        // argument (by creating another data object)
        send "eval" (makeDataObject [ makeSlot "activation" dObj ]) (lookupSlotValue "body" actv)
      ))
    ]
  )) ]

  
// ----------------------------------------------------------------------------
// Booleans and string comparison
// ----------------------------------------------------------------------------

// Booleans are objects with 'if' method that takes 'trueBlock' and 
// 'falseBlock' as arguments - and they call the right one!
let falseObj = makeDataObject [
  makeSlot "if" (makeObject [] (
    ( exprImplicit |> exprSend "falseBlock" |> exprSend "value" )
  ))
]

// TODO: Implement the 'true' Boolean
let trueObj = makeDataObject [
    makeSlot "if" (makeObject [] (
        (exprImplicit |> exprSend "trueBlock" |> exprSend "value")
    ))
]

stringPrototype |> addSlot "equals" (makeNativeMethod(fun arcd ->
  // TODO: equals operation on strings takes 'other' string as
  // argument. It returns 'trueObj' or 'falseObj'. To compare strings,
  // lookup 'self*' and 'other' and get their string values.
  let other = getStringValue (lookupSlotValue "other" arcd)
  let self = getStringValue (lookupSlotValue "self*" arcd)
  if other = self then trueObj else falseObj
))


// ----------------------------------------------------------------------------
// Demo - Meowing cats
// ----------------------------------------------------------------------------


// A cat has a sound and prints it when you 'call' it with 
// 'callee' argument that matches its name. For other names,
// it does not do anything. In Self:
//
//   (| sound = 'Meow'. call = ( 
//       (self name equals: callee) 
//          ifTrue: (self sound print)
//          False: (| |) ) |)
//
let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
  makeSlot "call" (makeObject [] (
    exprSelf 
    |> exprSend "name" 
    |> exprSendWith "equals" [ 
        "other", exprImplicit |> exprSend "callee" ]
    |> exprSendWith "if" [
        "trueBlock", exprBlock (exprSelf |> exprSend "sound" |> exprSend "print")
        "falseBlock", exprBlock (exprNew [])
    ]
  ))
]

// Two sample cats
let larry = makeDataObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
]
let cheshire = makeDataObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Cheshire")
]

// Larry meows only if he receives 'Larry'
larry |> send "call" (makeDataObject [ makeSlot "callee" (makeString "Larry") ])
larry |> send "call" (makeDataObject [ makeSlot "callee" (makeString "Cheshire") ])

// Cheshire meows only if she receives 'Cheshire'
cheshire |> send "call" (makeDataObject [ makeSlot "callee" (makeString "Larry") ])
cheshire |> send "call" (makeDataObject [ makeSlot "callee" (makeString "Cheshire") ])
