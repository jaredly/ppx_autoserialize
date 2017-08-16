
open Migrate_parsetree.Ast_403;

/* utils */
let fail loc txt => raise (Location.Error (Location.error ::loc txt));

let left txt => Ast_helper.Pat.var (Location.mknoloc txt);
let simple txt => Ast_helper.Exp.ident (Location.mknoloc txt);

let rec suffixify lident suffix => Longident.(switch lident {
| Lident x => Lident (x ^ suffix)
| Ldot x y => Ldot x (y ^ suffix)
| Lapply a b => Lapply a (suffixify b suffix)
});

let rec chainExpressions expressions => {
  switch expressions {
  | [] => assert false
  | [one] => one
  | [one, ...rest] => Ast_helper.Exp.sequence one (chainExpressions rest)
  }
};

let paramd_fun param_names body => {
  open Parsetree;
  open Longident;
  open Ast_helper;

  List.fold_right
  (fun name body => {
    Exp.fun_
    Asttypes.Nolabel
    None
    (Pat.var (Location.mknoloc (name ^ "_converter")))
    body
  })
  param_names
  body
};

type config = {
  prefix: Parsetree.structure,
  suffix: string,
  variant: (Parsetree.core_type => Parsetree.expression) => list Parsetree.constructor_declaration => Parsetree.expression,
  record: (Parsetree.core_type => Parsetree.expression) => list Parsetree.label_declaration => Parsetree.expression,
};

let rec core_type_converter suffix typ => {
  open Parsetree;
  open Ast_helper;
  switch typ.ptyp_desc {
  | Ptyp_constr {txt} args => {
    let main = simple (suffixify txt suffix);
    if (args === []) {
      main
    } else {
      Exp.apply main (List.map (fun arg => (Asttypes.Nolabel, core_type_converter suffix arg)) args)
    }
  }
  | Ptyp_var name => {
    simple (Lident (name ^ "_converter"))
  }
  | _ => fail typ.ptyp_loc "Nopes"
  }
};

let make_converters configs {Parsetree.ptype_name: {txt}, ptype_params, ptype_kind, ptype_manifest, } => {
  let param_names = List.map
  (fun (typ, _) => {
    switch typ.Parsetree.ptyp_desc {
    | Ptyp_var text => text
    | _ => assert false
    }
  })
  ptype_params;

  List.map
  (fun {suffix, variant, record} => {
    let right = switch ptype_manifest {
    | Some typ => {
      core_type_converter suffix typ
    }
    | None => switch ptype_kind {
      | Ptype_abstract => [%expr fun value => "type is abstract"]
      | Ptype_variant constructors => variant (core_type_converter suffix) constructors /* TODO names */
      | Ptype_record labels => record (core_type_converter suffix) labels
      | Ptype_open => [%expr fun value => "type is open & cannot be converted"]
      }
    };

    Ast_helper.Str.value Nonrecursive [
      Ast_helper.Vb.mk
      (left (txt ^ suffix))
      (paramd_fun param_names right)
    ]
  })
  configs
};

let mapper configs => Parsetree.{
  ...Ast_mapper.default_mapper,

  structure: fun mapper structure => {
    let rec loop items => {
      switch items {
      | [] => []
      | [{pstr_desc: Pstr_type isrec declarations} as item, ...rest] => {
        let converters = (List.map (make_converters configs) declarations |> List.concat);
        [item, ...List.append converters (loop rest)]
      }
      | [item, ...rest] => [mapper.structure_item mapper item, ...loop rest]
      }
    };
    let items = loop structure;
    List.append
    (List.concat (List.map (fun config => config.prefix) configs))
    items
  },
};

let jsonConfig = {
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

let devtoolsConfig = {
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


