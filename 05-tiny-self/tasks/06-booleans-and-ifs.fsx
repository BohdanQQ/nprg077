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
let addSlot n contents obj = failwith "Implemented in step 2"
let addParentSlot n contents obj = failwith "Implemented in step 2"
let cloneObject obj = failwith "Implemented in step 2"

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup msg obj : list<Objekt * Slot> = failwith "implemented in step 3"
and parentLookup msg obj : list<Objekt * Slot> = failwith "implemented in step 2"

let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  failwith "implemented in step 4"
let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
  failwith "implemented in step 3"

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// NOTE: Implemented in step #2
let empty = failwith<Objekt> "implemented in step 2"
let printCode = failwith<Objekt> "implemented in step 2"
let stringPrototype = failwith<Objekt> "implemented in step 2"
let makeString s = failwith "implemented in step 2"

// ----------------------------------------------------------------------------
// Cloning and assignments
// ----------------------------------------------------------------------------

let cloneMethod = failwith<Objekt> "implemented in step 3"
let clonablePrototype = failwith<Objekt> "implemented in step 3"

let assignmentMethod n = failwith "implemented in step 3"
let makeAssignmentSlot n = failwith "implemented in step 3"

// ----------------------------------------------------------------------------
// TinySelf code representation & interpreter
// ----------------------------------------------------------------------------

let exprSelf = failwith<Objekt> "implemented in step 4"
let exprString (s:string) = failwith "implemented in step 4"
let exprSend msg rcv = failwith "implemented in step 4"

let exprImplicit = failwith<Objekt> "Implemented in step 5"
let exprSeq e1 e2 = failwith<Objekt> "Implemented in step 5"
let exprSendWith msg args rcv = failwith<Objekt> "Implemented in step 5"
  
let exprNew slots = makeDataObject [
  makeSlot "slots" (makeDataObject [ for k, v in slots -> makeSlot k v ])
  makeSlot "eval" (makeNativeMethod (fun msg ->
    // TODO: Expression that represents the creation of a new data object.
    // To evaluate this, retrieve & evaluate all expressions in 'slots'
    // (similar to evaluation of arguments in 'exprSendWith') and 
    // construct & return new data object
    failwith "not implemented"
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
      makeSlot "value" (makeNativeMethod(fun arcd ->
        // The activation record for the body contains 'self*' from the
        // declaration site, and can access 'args' from both the declaration
        // site (static scope) and the args passed to the method (dynamic scope)
        let actVal = actVal |> cloneObject
        let self = actVal |> lookupSlotValue "self*"
        let args1 = actVal |> lookupSlotValue "args*" 
        let args2 = arcd |> lookupSlotValue "args*" 
        
        // TODO: Last bit - create data object with 3 parent slots 
        // (args1*, args2* and self*) storing the above 3 objects,
        // fetch the 'body' expression and send it an 'eval' message.
        // To call 'eval' pass it the newly created object as 'activation'
        // argument (by creating another data object)
        failwith "not implemented"
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
let trueObj = failwith<Objekt> "not implemented"

stringPrototype |> addSlot "equals" (makeNativeMethod(fun arcd ->
  // TODO: equals operation on strings takes 'other' string as
  // argument. It returns 'trueObj' or 'falseObj'. To compare strings,
  // lookup 'self*' and 'other' and get their string values.
  failwith "not implemented"
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
