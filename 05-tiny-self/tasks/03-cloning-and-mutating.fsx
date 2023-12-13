// ----------------------------------------------------------------------------
// 03 - Cloning and mutating TinySelf objects
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

// TODO: Modify 'send' and 'eval' to also take message send arguments.
// In Self, the arguments are copied into the activation record. 
// In TinySelf, we use simpler trick - just make the 'args' object 
// another parent of the activation record! Lookup for argument name 
// in the activation record will then give us the value.
// NOTE: The object newly returned from 'lookup' should be ignored.
// BEWARE: All arguments are 'Objekt' so it is easy to swap them!! 

let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue.Code with
    | None -> slotValue
    | Some code -> 
        match code.Special with
          | Some(Native f) -> 
            let clone = cloneObject code
            clone |> addParentSlot "self*" instance
            clone |> addParentSlot "args*" args
            f clone
          | _ -> failwith "Non-native methods not implemented" 
  


let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt = 
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
  // TODO: The activation record contains a slot 'self*' which is the
  // target object. Use lookup to get it, clone it & retrn it!
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

// Creates an assignment slot for a slot named 'n'
let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }
  
// ----------------------------------------------------------------------------
// Tests - cloning and modifying cats
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let mogscats = makeDataObject [
  makeSlot "book" (makeString "Mog's Family of Cats")
  // NOTE: This allows us to rename the book (probably not
  // something you'd want to do, but for illustration purposes...)
  makeAssignmentSlot "book"
]
let mog = makeDataObject [
  // NOTE: Mog is now also clonable and has assignment slot "name:"
  makeParentSlot "parent*" cat
  makeParentSlot "clonable*" clonablePrototype
  makeParentSlot "fictional*" mogscats
  makeSlot "name" (makeString "Mog")
  makeAssignmentSlot "name"
]

// NOTE: We now pass empty arguments to all of the message sends
mog |> send "name" empty |> send "print" empty
mog |> send "sound" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty

// NOTE: Clone Ginger and print its name & book
let ginger = mog |> send "clone" empty

ginger |> send "name" empty |> send "print" empty
ginger |> send "book" empty |> send "print" empty

// TODO: Write code to change the name of 'ginger' to "Ginger"!
// (send message "name:" with arument containing slot 'new' with the new value)
ginger 
|> send "name:" (makeDataObject [makeSlot "new" (makeString "Ginger!")]) |> send "name" empty |> send "print" empty

// TODO: Write code to change the book of 'ginger' to "Goodbye, Mog"!
ginger 
|> send "book:" (makeDataObject [makeSlot "new" (makeString "Goodbye, Mog")]) |> send "book" empty |> send "print" empty

// TODO: What do we get if we run the following now?
mog |> send "name" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty
ginger |> send "name" empty |> send "print" empty
ginger |> send "book" empty |> send "print" empty
