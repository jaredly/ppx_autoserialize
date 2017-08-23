
open SharedTypes;
open Utils;
open Migrate_parsetree.Ast_403;

let module Json = Json;
let module YoJson = YoJson;
let module Utils = Utils;
let module Types = Types;
let module Devtools = Devtools;

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
  /* TODO serlize the AST & show it here for debugging */
  | _ => [%expr fun _ => failwith "Unexpected core type, cannot convert"]
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
      [%expr fun value => [%e core_type_converter suffix typ] value]
    }
    | None => switch ptype_kind {
      | Ptype_abstract => [%expr fun value => "type is abstract & cannot be converted"]
      | Ptype_variant constructors => variant (core_type_converter suffix) constructors txt
      | Ptype_record labels => record (core_type_converter suffix) labels txt
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

  payload: fun mapper payload => payload,

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
