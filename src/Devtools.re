
open SharedTypes;
open Utils;
open Migrate_parsetree.Ast_403;

let config = {
  prefix: [%str
    external to_devtools: 'a => Js.t {.} = "%identity";
    let int__to_devtools = to_devtools;
    let float__to_devtools = to_devtools;
    let string__to_devtools = to_devtools;
    let boolean__to_devtools = to_devtools;
    let list__to_devtools convert items => {
      "$bs": "list",
      "items": List.map convert items |> Array.of_list
    } |> to_devtools;
    let array__to_devtools convert items => Array.map convert items |> to_devtools;
    let option__to_devtools convert item => switch item {
    | None => {"$bs": "optional", "empty": true, "value": Js.Null.empty}
    | Some x => {"$bs": "optional", "empty": false, "value": Js.Null.return (convert x)}
    } |> to_devtools;
  ],
  suffix: "__to_devtools",
  typ: To [%type: Js.t {.}],

  variant: fun core_type_converter constructors name => {
    open Parsetree;
    open Ast_helper;
    open Longident;
    let cases = List.map
    (fun {pcd_name: {txt, loc}, pcd_args} => {
      let strConst = (Exp.constant (Pconst_string txt None));
      let lid = (Location.mknoloc (Lident txt));
      switch pcd_args {
      | Pcstr_tuple types => {
        switch types {
          | [] => Exp.case (Pat.construct lid None) [%expr
            {
              "$bs": "variant",
              "type": [%e Utils.strConst name],
              "constructor": [%e strConst],
              "arguments": [||]
            } |> to_devtools ]
          | _ => {
            let items = List.mapi
            (fun i typ => Utils.patVar ("arg" ^ (string_of_int i)))
            types;
            let args = switch items {
            | [] => None
            | [single] => Some single
            | _ => Some (Pat.tuple items)
            };

            let pat = Pat.construct lid args;
            let values = List.mapi
            (fun i typ => {
              let larg = Utils.expIdent ("arg" ^ (string_of_int i));
              [%expr [%e core_type_converter typ] [%e larg]]
            })
            types;
            let expr = [%expr {
              "$bs": "variant",
              "type": [%e Utils.strConst name],
              "constructor": [%e strConst],
              "arguments": [%e Utils.list values] |> Array.of_list,
            } |> to_devtools ];
            Exp.case pat expr
          }
        }
      }
      /* This isn't supported in 4.02 anyway */
      | Pcstr_record labels => Utils.fail loc "Nope record labels"
      }
    })
    constructors;
    Exp.fun_
    Asttypes.Nolabel
    None
    (Utils.patVar "value")
    (Exp.match_ [%expr value] cases)
  },

  record: fun core_type_converter labels name => {
    open Parsetree;
    open Longident;
    open Ast_helper;
    let strConst txt => Exp.constant (Pconst_string txt None);

    let sets = List.map
    (fun {pld_name: {txt}, pld_type} => {
      let value = Exp.field [%expr value] (Location.mknoloc (Lident txt));
      [%expr Js.Dict.set result [%e strConst txt]
        ([%e core_type_converter pld_type] [%e value])]
    })
    labels;

    let body = List.append
      sets
      [
        [%expr {"$bs": "record", "type": [%e strConst name], "attributes": result |> to_devtools} |> to_devtools]
      ]
    |> chainExpressions;

    let body = Exp.let_ Nonrecursive [
      Ast_helper.Vb.mk
      (left "result")
      [%expr Js.Dict.empty ()]
    ]
    body;

    Exp.fun_
      Asttypes.Nolabel
      None
      (Pat.var (Location.mknoloc "value"))
      body;
  },
};

