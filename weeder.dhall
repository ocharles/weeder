let Module
    : Type
    = { unit-id : Text, module : Text }

let Declaration
    : Type
    = Module ⩓ { symbol : Text }

let Root
    : Type
    = < Declaration : Declaration | Module : Module >

in  { ignore = [] : List Declaration
    , roots =
      [ Root.Declaration
          { unit-id = "weeder-1.0-inplace"
          , module = "Weeder.Main"
          , symbol = "main"
          }
      ]
    , strict = True
    , type-class-roots = True
    }
