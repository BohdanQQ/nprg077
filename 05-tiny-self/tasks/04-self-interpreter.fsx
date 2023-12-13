// ----------------------------------------------------------------------------
// 04 - Representing & interpreting TinySelf expressions
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

// TODO: If 'slotValue' has some non-native 'Code', we want to evaluate it.
// But it will be easier to add this later, so copy code from step 3 and then
// return to it later (there is a TODO below telling you to do this).
//
// If the slot contains 'Some(code)', we run it by sending the 'eval' message
// to the 'code'. The method takes the activation record (the same as in the
// case of native code) as the argument, so create data object with 'activation' 
// slot and pass the as argument when calling 'eval'.
// (to call 'send', you will need to use 'let rec .. and ..' here)
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
  // NOTE: We ignore the object returned by 'lookup' here.
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// NOTE: Implemented in step #2
let empty : Objekt = makeDataObject []

let printCode = makeNativeMethod (fun arcd ->
  printfn "%A" (getStringValue arcd)
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

// NOTE: All code is represented as objects with 'eval' method. 
// The 'eval' method receives argument 'evalTarget' which represents 
// the object of the method containing the code that is being invoked.
// The 'eval' method is a native method implemented as F# function. We
// store the parameters of the expressions in the objects themselves 
// (and not in F# closures) to keep more things in the TinySelf world.


// TODO: 'self' expression has no arguments and needs no state. When 
// evaluated, it needs to get the original 'activation' we constructed
// when calling 'eval' - this is done by looking up 'activation' slot
// in the activation record with which the method is called - and then 
// from that, we can get the 'self*' slot.
// 
// NOTE: This is a bit confusing - the F# function we write gets 'arcd'
// which is the activation record for the 'eval' call. But this in turn 
// contains activation record 'activation' which is the activation record
// for the message send that our interpreter is handling!
let exprSelf = makeDataObject [
    makeSlot "eval" (makeNativeMethod (fun arcd -> 
       lookupSlotValue "self*" (lookupSlotValue "activation" arcd)
    ))
]

// DEMO: 'string' expression stores the string value in a slot 'str'. When 
// evaluated, it fetches 'str' (slot value) from the activation record.
let exprString (s:string) = makeDataObject [ 
  makeSlot "string" (makeString s) 
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    lookupSlotValue "string" arcd
  )) ]

let exprSend msg rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun arcd -> 
    // TODO: To evalaute 'send' expression, we need to:
    // * Get 'activation' (activation record of the method call we are 
    ///  interpreting) from the activation record 'arcd' and create
    let actr = lookupSlotValue "activation" arcd
    //   a new data object with this as the 'activation' to be used
    //   as an argument of recursive 'eval' call(s) later
    let dObj = makeDataObject [ makeSlot "activation" actr ]
    // * Get the string value of 'msg' slot (lookup from the 'acrd')
    let msgName = getStringValue (lookupSlotValue "msg" arcd)
    // * Get the receiver expression (from the 'acrd')
    //   and evaluate it by send it 'eval' with the data object 
    //   (containing 'activation') as argument
    let receiver = lookupSlotValue "receiver" arcd
    let evalueatedRcvr = send "eval" dObj receiver
    // * Send 'msg' to the recursively evaluated receiver object!
    send msgName empty evalueatedRcvr 
  )) ]

// ----------------------------------------------------------------------------
// Tests - hello world (finally!)
// ----------------------------------------------------------------------------

// TinySelf code that sends "print" to a Hello world string
let helloCode =
  exprString "Hello world!!" |> exprSend "print" 

// Run it! We need to create arguments for 'eval' with 'activation'
// but since we are not using it (no exprSelf), it can be empty.
let emptySelf = makeDataObject [makeSlot "activation" empty]
helloCode |> send "eval" emptySelf |> ignore

// TODO: Now go back to the missing case in 'eval'. 
// We now add code as methods to a TinySelf object.



// Object with 'hello' method that prints hello world!
let helloObj = makeDataObject [ 
  makeSlot "hello" (makeObject [] helloCode) 
]
helloObj |> send "hello" empty |> ignore

// TODO: A more interesting example! Create an object with 
// string slot 'sound' (Meow! if you like cats) and a 'speak'
// method that sends 'sound' to self (use exprSelf) and then
// sends 'print' to the result to print it.

let soundCode =
  exprSelf |> exprSend "sound" |> exprSend "print" 

let meowCode =
  exprString "Meow!" |> exprSend "print" 

let animalObj = makeDataObject [ 
  makeSlot "sound" (makeString "Meow!") 
  makeSlot "speak" (makeObject [] soundCode) 
]

animalObj |> send "speak" empty |> ignore
