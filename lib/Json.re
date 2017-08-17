
open Types;
open Utils;
open Migrate_parsetree.Ast_403;

let stringify = {
  prefix: [%str
    let int__'bs'to_json x => Js.Json.number (float_of_int x);
    let float__'bs'to_json = Js.Json.number;
    let list__'bs'to_json convert items => Js.Json.array (Array.of_list (List.map convert items));
    let string__'bs'to_json = Js.Json.string;
    let array__'bs'to_json convert items => Js.Json.array (Array.map convert items);
    let boolean__'bs'to_json = Js.Json.boolean;
  ],
  suffix: "__'bs'to_json",
  variant: fun core_type_converter constructors => {
    simple (Longident.Lident "jkjk")
  },

  record: fun core_type_converter labels => {
    open Parsetree;
    open Longident;
    open Ast_helper;

    let sets = List.map
    (fun {pld_name: {txt}, pld_type} => {
      let value = Exp.field [%expr value] (Location.mknoloc (Lident txt));
      let strConst = Exp.constant (Pconst_string txt None);
      [%expr Js.Dict.set result [%e strConst]
        ([%e core_type_converter pld_type] [%e value])]
    })
    labels;

    let body = List.append
      sets
      [
        [%expr Js.Json.object_ result]
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
  }
};

let parse = {
  prefix: [%str
    let int__'bs'from_json x => switch (Js.Json.classify x) {
    | JSONNumber n => Some (int_of_float n)
    | _ => None
    };
    let float__'bs'from_json x => switch (Js.Json.classify x) {
    | JSONNumber n => Some n
    | _ => None
    };
    let list__'bs'from_json convert items => switch (Js.Json.classify x) {
    | JSONArray arr => {
      try {
        let items = Array.map (fun item => {
          switch (convert item) {
          | Some x => x
          | None => failwith "Item failed to parse"
          }
        }) arr;
        Some (Array.to_list items)
      } {
        | _ => None
      }
    }
    | _ => None
    };
    let string__'bs'from_json value => switch (Js.Json.classify x) {
    | JSONString str => Some str
    | _ => None
    };
    let array__'bs'from_json convert items => switch (Js.Json.classify x) {
    | JSONArray arr => {
      try {
        let items = Array.map (fun item => {
          switch (convert item) {
          | Some x => x
          | None => failwith "Item failed to parse"
          }
        }) arr;
        Some items
      } {
        | _ => None
      }
    }
    | _ => None
    };
    let boolean__'bs'from_json value => switch (Js.Json.classify x) {
    | JSONFalse => Some false
    | JSONTrue => Some true
    | _ => None
    };
  ],
  suffix: "__'bs'from_json",
  variant: fun core_type_converter constructors => {
    simple (Longident.Lident "jkjk")
  },

  record: fun core_type_converter labels => {
    open Parsetree;
    open Longident;
    open Ast_helper;

    let body = Exp.record
    (List.map 
    (fun {pld_name: {txt}} => (
      (Location.mknoloc (Lident txt)),
      Exp.ident (Location.mknoloc (Lident (txt ^ "_extracted")))
    ))
    labels)
    None;

    let body = List.fold_right
    (fun {pld_name: {txt}, pld_type} body => {
      let strConst = Exp.constant (Pconst_string txt None);
      let strPat = Pat.var (Location.mknoloc (txt ^ "_extracted"));

      [%expr switch (Js.Dict.get value [%e strConst]) {
      | None => None
      | Some attr => switch ([%e core_type_converter pld_type] attr) {
        | None => None
        | Some [%p strPat] => [%e body]
        }
      }]
    })
    labels
    [%expr Some [%e body]];

    let body = [%expr switch (Js.Json.classify value) {
    | JSONObject value => [%e body]
    | _ => None
    }];

    Exp.fun_
      Asttypes.Nolabel
      None
      (Pat.var (Location.mknoloc "value"))
      body;
  }
};
