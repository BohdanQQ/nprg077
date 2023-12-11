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
let empty = failwith "implemented in step 2"
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
    // 'eval' calls (as in 'exprSend'), recursively evaluate 'e1',
    // ignore the result, then recursively evaluate 'e2' & return the result
    failwith "not implemented"
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
    failwith "not implemented"
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
    ( exprString "!!" |> exprSend "print" )
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
