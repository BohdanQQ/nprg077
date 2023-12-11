// ----------------------------------------------------------------------------
// 07 - Treating objects as lists and adding map
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
  
let exprNew slots = failwith "implemented in step 6"
let exprBlock ebody = failwith "implemented in step 6"

// ----------------------------------------------------------------------------
// Booleans and string comparison
// ----------------------------------------------------------------------------

let falseObj = failwith<Objekt> "implmented in step 6"
let trueObj = failwith<Objekt> "implmented in step 6"

stringPrototype |> addSlot "equals" (failwith "implmented in step 6")
  
// ----------------------------------------------------------------------------
// Tests - lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

let animal = makeDataObject [
  makeSlot "say" (makeObject [] (
    exprSelf  |> exprSend "sound" |> exprSend "print"
  ))
]

let larry = makeDataObject [ 
  makeParentSlot "parent*" animal
  makeSlot "sound" (makeString "Meow")
]
let cheshire = makeDataObject [ 
  makeParentSlot "parent*" animal
  makeSlot "sound" (makeString "We are all mad!")
]
let dasenka = makeDataObject [ 
  makeParentSlot "parent*" animal
  makeSlot "sound" (makeString "Haf haf haf!")
]

let listPrototype = makeDataObject [
  makeSlot "map" (makeNativeMethod(fun arcd -> 
    // TODO: Map operation returns a new object obtained by applying 'f'
    // to all non-parent slots of an object. Parent slots are just 
    // copied to the result unmodified. That is something like:
    //
    //   (| s1 = os1 ... sn = osn. p1* = op1. p2* = op2 ... |) map: f = 
    //     (| s1 = f os1 ... sn = f osn. p1* = op1. p2* = op2 ... |)
    // 
    let self = lookupSlotValue "self*" arcd
    let f = lookupSlotValue "f" arcd
    
    // TODO: Create a new data object with the result by iterating over
    // the slots of 'self'. 'f' is a block and can be evaluated by sending
    // it the 'value' message. When sending the message, give it the current
    // slot value as 'element' parameter.
    failwith "not implemented"
  ))  
]

let animals = makeDataObject [
  makeParentSlot "list*" listPrototype
  makeSlot "_1" larry
  makeSlot "_2" cheshire
  makeSlot "_3" dasenka
]

let demo = makeDataObject [
  makeSlot "animals" animals
  makeSlot "main" (makeObject [] (
    exprSelf |> exprSend "animals" |> exprSendWith "map" [
      "f", exprBlock (exprImplicit |> exprSend "element" |> exprSend "say")
    ]
  ))
 ]

demo |> send "main" empty |> ignore
