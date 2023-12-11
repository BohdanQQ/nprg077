module Self
// ----------------------------------------------------------------------------
// 08 - Creating web-based visualizers
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
// Creating some web-based visualizers!
// ----------------------------------------------------------------------------

// NOTE: Add 'render' method for string - it is rendered as just string
stringPrototype
|> addSlot "render" (makeObject [] exprSelf)

// NOTE: Add 'render' method for animals - they produce more complex HTML
let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
  makeSlot "say" (makeObject [] (
    exprSelf  |> exprSend "sound" |> exprSend "print"
  ))
  makeSlot "render" (makeObject [] (
    exprNew [
      "tag", exprString "div"
      "children", exprNew [
        "1", exprNew [ 
          "tag", exprString "h3" 
          "children", exprNew [ "1", exprSelf |> exprSend "name" ]
        ]
        "2", exprNew [ 
          "tag", exprString "marquee" 
          "attrs", exprNew [ "style", exprString "color:purple" ]
          "children", exprNew [ "1", exprSelf |> exprSend "sound" ]
        ]
        // TODO: Modify the rendering code to show cat pictures!
        // (You can see above how to create tags with attributes...)
      ]
    ]
  ))
]

// Some sample cats
let larry = makeDataObject [ 
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Larry")
  makeSlot "image" (makeString "larry.jpg")
]
let cheshire = makeDataObject [ 
  makeParentSlot "parent*" cat
  makeSlot "sound" (makeString "We are all mad!")
  makeSlot "name" (makeString "Cheshire cat")
  makeSlot "image" (makeString "cheshire.jpg")
]
let dasenka = makeDataObject [ 
  makeParentSlot "parent*" cat
  makeSlot "name" (makeString "Mog")
  makeSlot "image" (makeString "mog.jpg")
]

let animals = makeDataObject [
  makeSlot "_1" larry
  makeSlot "_2" cheshire
  makeSlot "_3" dasenka
]


// Another demo - just a simple TinySelf hello world object 
let greeterPrototype = makeDataObject [
  makeSlot "greet" (makeObject [] (
    exprSelf |> exprSend "greeting" |> exprSend "print"
  ))
]
let greeter = makeDataObject [
  makeSlot "parent*" greeterPrototype
  makeSlot "greeting" (makeString "Hello world!")
]


// Demo object that provides access to other things 
let demo = makeDataObject [
  makeSlot "animals" animals
  makeSlot "greeter" greeter
]



