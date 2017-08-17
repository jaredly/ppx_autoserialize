
open Types;
open Utils;

let config = {
  prefix: [%str
    type devtools; /* TODO this doesn't work across modules */
    external to_devtools: 'a => devtools = "%identity";
    let int__'bs'to_devtools = to_devtools;
    let float__'bs'to_devtools = to_devtools;
    let string__'bs'to_devtools = to_devtools;
    let boolean__'bs'to_devtools = to_devtools;
    let list__'bs'to_devtools convert items => {"$bs": "list", "items": List.map convert items} |> to_devtools;
    let array__'bs'to_devtools convert items => Array.map convert items |> to_devtools;
  ],
  suffix: "__'bs'to_devtools",
  variant: fun core_type_converter constructors => simple (Lident "Nope"),
  record: fun core_type_converter labels => simple (Lident "Nope"),
};

