open SharedTypes;

open Utils;

/* open Migrate_parsetree.Ast_403; */

module Json = Json;

module YoJson = YoJson;

module Utils = Utils;

module Types = Types;

module Devtools = Devtools;

let rec core_type_converter = (suffix, typ) =>
  Parsetree.(
    Ast_helper.(
      switch typ.ptyp_desc {
      | Ptyp_constr({txt}, args) =>
        let main = simple(suffixify(txt, suffix));
        if (args === []) {
          main;
        } else {
          Exp.apply(
            main,
            List.map((arg) => ("", core_type_converter(suffix, arg)), args)
          );
        };
      | Ptyp_var(name) =>
        simple(
          Lident(name ++ "_converter")
        ) /* TODO serlize the AST & show it here for debugging */
      | _ => [%expr ((_) => failwith("Unexpected core type, cannot convert"))]
      }
    )
  );

let make_signatures =
    (
      ~autoAll,
      configs,
      {
        Parsetree.ptype_name: {txt} as name,
        ptype_params,
        ptype_kind,
        ptype_manifest,
        ptype_attributes
      }
    ) => {
    let param_names =
      List.map(
        ((typ, _)) =>
          switch typ.Parsetree.ptyp_desc {
          | Ptyp_var(text) => text
          | _ => assert false
          },
        ptype_params
      );
    let thisType =
      Ast_helper.Typ.constr(
        Location.mknoloc(Longident.Lident(txt)),
        List.map(((typ, _)) => typ, ptype_params)
      );
    List.fold_left(
      (results, {suffix, variant, record, typ, decorator}) => {
        let generate = autoAll || List.exists((({Asttypes.txt}, _)) => txt == decorator, ptype_attributes);

        if (generate) {
        let right =
          switch typ {
          | To(typ) => Ast_helper.Typ.arrow("", thisType, typ)
          | From(typ) => Ast_helper.Typ.arrow("", typ, [%type : option([%t thisType])])
          };
        [Ast_helper.Sig.value(
          Ast_helper.Val.mk(Location.mknoloc(txt ++ suffix), paramd_type(param_names, right, typ))
        ), ...results];
  } else {
    results
  }
      },
      [],
      configs
    );
    };

let make_converters =
    (
      ~autoAll,
      configs,
      {Parsetree.ptype_name: {txt}, ptype_params, ptype_kind, ptype_manifest, ptype_attributes}
    ) =>
  switch ptype_attributes {
  | [({txt: "noserialize"}, _)] => []
  | _ =>
    let param_names =
      List.map(
        ((typ, _)) =>
          switch typ.Parsetree.ptyp_desc {
          | Ptyp_var(text) => text
          | _ => assert false
          },
        ptype_params
      );
    List.fold_left(
      (results, {suffix, variant, record, decorator}) => {
        let generate = autoAll || List.exists((({Asttypes.txt}, _)) => txt == decorator, ptype_attributes);

        if (generate) {
          let right =
            switch ptype_manifest {
            | Some((typ)) => [%expr ((value) => [%e core_type_converter(suffix, typ)](value))]
            | None =>
              switch ptype_kind {
              | Ptype_abstract => [%expr ((value) => "type is abstract & cannot be converted")]
              | Ptype_variant(constructors) =>
                variant(core_type_converter(suffix), constructors, txt)
              | Ptype_record(labels) => record(core_type_converter(suffix), labels, txt)
              | Ptype_open => [%expr ((value) => "type is open & cannot be converted")]
              }
            };
          [Ast_helper.Str.value(
            Nonrecursive,
            [Ast_helper.Vb.mk(left(txt ++ suffix), paramd_fun(param_names, right))]
          ), ...results];
        } else {
          results
        }
      },
      [],
      configs
    ) |> List.rev;
  };

let mapper = (~autoAll=false, configs) =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    payload: (mapper, payload) => payload,
    signature: (mapper, signature) => {
      let rec loop = (items) =>
        switch items {
        | [] => []
        | [{psig_desc: Psig_type(declarations)} as item, ...rest] =>
          let converters = List.map(make_signatures(~autoAll, configs), declarations) |> List.concat;
          [item, ...List.append(converters, loop(rest))];
        | [item, ...rest] => [mapper.signature_item(mapper, item), ...loop(rest)]
        };
      loop(signature);
    },
    structure: (mapper, structure) => {
      let rec loop = (items) =>
        switch items {
        | [] => []
        | [{pstr_desc: Pstr_type(declarations)} as item, ...rest] =>
          let converters = List.map(make_converters(~autoAll, configs), declarations) |> List.concat;
          [item, ...List.append(converters, loop(rest))];
        | [item, ...rest] => [mapper.structure_item(mapper, item), ...loop(rest)]
        };
      let items = loop(structure);
      List.append(List.concat(List.map((config) => config.prefix, configs)), items);
    }
  };
