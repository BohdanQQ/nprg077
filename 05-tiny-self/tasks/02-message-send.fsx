// ----------------------------------------------------------------------------
// 02 - Implementing (basic) message sending
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


// Native method has a special object (F# function) as code
let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

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

let rec lookup (msg:string) (obj:Objekt) : list<Slot> =  
   match obj.Slots |> List.filter (fun s -> s.Name = msg) with 
    | [] -> parentLookup msg obj
    | s -> s
and parentLookup msg obj = obj.Slots |> List.filter (fun s -> s.IsParent ) |> List.map (fun s -> lookup msg s.Contents) |> List.concat

// See also �3.3.7 (https://handbook.selflanguage.org/SelfHandbook2017.1.pdf)
//
// Note that we do not need special "primitive sends". Instead, we have special
// objects and so we need to run the "native" method when we it is called.
//
// Also not that we do not yet support passing arguments to methods!

let eval (slotValue:Objekt) (instance:Objekt) =
  // TODO: Implement the evaluation logic:
  // * If the 'slotValue' is a data object (has no 'Code') it is returned
  // * If the 'slotValue' has 'Code', we should invoke it. For now, we only
  //   handle the case where 'Code' is 'Special' and has 'Native' method.
  // * If the 'slotValue' has 'Code' that's not 'Special' fail (for now)
  //
  match slotValue.Code with
    | None -> slotValue
    | Some code -> 
        match code.Special with
          | Some(Native f) -> 
  // To run the method we need to clone the method object (using cloneObject),
  // make the receiver as the parent of the clone (using addParentSlot)
  // and invoke the native function with the clone as argument.
  //
  // NOTE: Why do we set the receiver as parent of the activation record?
  // We can then send messages to it directly to access the receiver's slots!
            let clone = cloneObject code
            clone |> addParentSlot "parent*" instance
            f clone
          | _ -> failwith "Non-native methods not implemented" 
  


let send (msg:string) (instance:Objekt) : Objekt = 
  match lookup msg instance with
    | [ obj ] -> eval obj.Contents instance
    | _ -> failwithf "Message '%s' not supported by the objekt %A!" msg instance

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  | [ { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

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
// Tests - still lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let wonderland = makeDataObject [
  makeSlot "book" (makeString "Alice in Wonderland")
]

let larry = makeDataObject [
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
]

// Sending 'book' to 'larry' will now throw 'message not understood'!
larry |> send "name" |> send "print"
larry |> send "sound" |> send "print"
larry |> send "book" |> send "print"

let cheshire = makeDataObject [
    makeSlot "name" (makeString "Cheshire Cat")
    makeParentSlot "parent*" cat
    makeParentSlot "parent*" wonderland
]

// All of these should be OK!
cheshire |> send "name" |> send "print" 
cheshire |> send "sound" |> send "print"
cheshire |> send "book" |> send "print"