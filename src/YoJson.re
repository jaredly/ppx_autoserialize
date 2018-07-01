open SharedTypes;

open Utils;

/* open Migrate_parsetree.Ast_403; */

let stringify = {
  decorator: "to.yojson",
  prefix: [%str
    let int__to_yojson = (x) => `Int(x);
    let float__to_yojson = (x) => `Float(x);
    let list__to_yojson = (convert, items) => `List(List.map(convert, items));
    let string__to_yojson = (x) => `String(x);
    let array__to_yojson = (convert, items) => `List(Array.to_list(Array.map(convert, items)));
    let boolean__to_yojson = (x) => `Bool(x);
    let option__to_yojson = (convert, x) =>
      switch x {
      | None => `Null
      | Some((x)) => `List([convert(x)])
      }
  ],
  suffix: "__to_yojson",
  typ: To([%type : Yojson.Safe.json]),
  variant: (core_type_converter, constructors, name) => {
    open Parsetree;
    open Ast_helper;
    open Longident;
    let cases =
      List.map(
        ({pcd_name: {txt, loc}, pcd_args: types}) => {
          let strConst = Exp.constant(Const_string(txt, None));
          let lid = Location.mknoloc(Lident(txt));
          /* switch pcd_args {
          | Pcstr_tuple(types) => */
            switch types {
            | [] => Exp.case(Pat.construct(lid, None), [%expr `String([%e strConst])])
            | _ =>
              let items = List.mapi((i, typ) => Utils.patVar("arg" ++ string_of_int(i)), types);
              let args =
                switch items {
                | [] => None
                | [single] => Some(single)
                | _ => Some(Pat.tuple(items))
                };
              let pat = Pat.construct(lid, args);
              let values =
                List.mapi(
                  (i, typ) => {
                    let larg = Utils.expIdent("arg" ++ string_of_int(i));
                    [%expr [%e core_type_converter(typ)]([%e larg])];
                  },
                  types
                );
              let values = [[%expr `String([%e strConst])], ...values];
              Exp.case(pat, [%expr `List([%e Utils.list(values)])]);
            } /* This isn't supported in 4.02 anyway */
          /* | Pcstr_record(labels) => Utils.fail(loc, "Nope record labels") */
          /* }; */
        },
        constructors
      );
    Exp.fun_("", None, Utils.patVar("value"), Exp.match([%expr value], cases));
  },
  record: (core_type_converter, labels, name) => {
    open Parsetree;
    open Longident;
    open Ast_helper;
    let sets =
      List.map(
        ({pld_name: {txt}, pld_type}) => {
          let value = Exp.field([%expr value], Location.mknoloc(Lident(txt)));
          let strConst = Exp.constant(Const_string(txt, None));
          [%expr ([%e strConst], [%e core_type_converter(pld_type)]([%e value]))];
        },
        labels
      );
    Exp.fun_(
      "",
      None,
      Pat.var(Location.mknoloc("value")),
      [%expr `Assoc([%e Utils.list(sets)])]
    );
  }
};

let parse = {
  decorator: "to.yojson",
  prefix: [%str
    let int__from_yojson = (x) =>
      switch x {
      | `Int(n) => Some(n)
      | _ => None
      };
    let float__from_yojson = (x) =>
      switch x {
      | `Float(n) => Some(n)
      | _ => None
      };
    let list__from_yojson = (convert, items) =>
      switch items {
      | `List(arr) =>
        try {
          let items =
            List.map(
              (item) =>
                switch (convert(item)) {
                | Some((x)) => x
                | None => failwith("Item failed to parse")
                },
              arr
            );
          Some(items);
        } {
        | _ => None
        }
      | _ => None
      };
    let string__from_yojson = (value) =>
      switch value {
      | `String(str) => Some(str)
      | _ => None
      };
    let array__from_yojson = (convert, items) =>
      switch items {
      | `List(arr) =>
        try {
          let items =
            List.map(
              (item) =>
                switch (convert(item)) {
                | Some((x)) => x
                | None => failwith("Item failed to parse")
                },
              arr
            );
          Some(Array.of_list(items));
        } {
        | _ => None
        }
      | _ => None
      };
    let boolean__from_yojson = (value) =>
      switch value {
      | `Bool(v) => Some(v)
      | _ => None
      };
    let option__from_yojson = (convert, value) =>
      switch value {
      | `Null => Some(None)
      | `List([value]) =>
        switch (convert(value)) {
        | None => None
        | Some((x)) => Some(Some(x))
        }
      | _ => None
      }
  ],
  suffix: "__from_yojson",
  typ: From([%type : Yojson.Safe.json]),
  variant: (core_type_converter, constructors, name) => {
    open Parsetree;
    open Ast_helper;
    open Longident;
    let cases =
      List.map(
        ({pcd_name: {txt, loc}, pcd_args: types}) => {
        let strConst = Exp.constant(Const_string(txt, None));
        let patConst = [%pat ? `String([%p Pat.constant(Const_string(txt, None))])];
          let lid = Location.mknoloc(Lident(txt));
          /* switch pcd_args {
          | Pcstr_tuple(types) => */
            switch types {
            | [] => Exp.case(patConst, [%expr Some([%e Exp.construct(lid, None)])])
            | _ =>
              let items = List.mapi((i, typ) => Utils.patVar("arg" ++ string_of_int(i)), types);
              let pattern = [%pat ? `List([%p Utils.patList([patConst, ...items])])];
              let args =
                switch types {
                | [] => None
                | [_] => Some(Utils.expIdent("arg0"))
                | _ =>
                  Some(
                    Exp.tuple(
                      List.mapi((i, typ) => Utils.expIdent("arg" ++ string_of_int(i)), types)
                    )
                  )
                };
              let expr = Exp.construct(lid, args);
              let (body, _) =
                List.fold_right(
                  (typ, (body, i)) => (
                    [%expr
                      switch (
                        [%e core_type_converter(typ)](
                          [%e Utils.expIdent("arg" ++ string_of_int(i))]
                        )
                      ) {
                      | None => None
                      | Some(([%p Utils.patVar("arg" ++ string_of_int(i))])) => [%e body]
                      }
                    ],
                    i - 1
                  ),
                  types,
                  ([%expr Some([%e expr])], List.length(types) - 1)
                );
              Exp.case(pattern, body);
            } /* This isn't supported in 4.02 anyway */
          /* | Pcstr_record(labels) => Utils.fail(loc, "Nope record labels")
          }; */
        },
        constructors
      );
    let cases = List.append(cases, [Exp.case(Pat.any(), [%expr None])]);
    Exp.fun_("", None, Utils.patVar("value"), Exp.match([%expr value], cases));
  },
  record: (core_type_converter, labels, name) => {
    open Parsetree;
    open Longident;
    open Ast_helper;
    let body =
      Exp.record(
        List.map(
          ({pld_name: {txt}}) => (
            Location.mknoloc(Lident(txt)),
            Exp.ident(Location.mknoloc(Lident(txt ++ "_extracted")))
          ),
          labels
        ),
        None
      );
    let body =
      List.fold_right(
        ({pld_name: {txt}, pld_type}, body) => {
          let strConst = Exp.constant(Const_string(txt, None));
          let strPat = Pat.var(Location.mknoloc(txt ++ "_extracted"));
          [%expr
            switch (get(items, [%e strConst])) {
            | None => None
            | Some((attr)) =>
              switch ([%e core_type_converter(pld_type)](attr)) {
              | None => None
              | Some(([%p strPat])) => [%e body]
              }
            }
          ];
        },
        labels,
        [%expr Some([%e body])]
      );
    let body = [%expr
      {
        let rec get = (items, name) =>
          switch items {
          | [] => None
          | [(attr, value), ..._] when attr == name => Some(value)
          | [_, ...rest] => get(rest, name)
          };
        switch value {
        | `Assoc(items) => [%e body]
        | _ => None
        };
      }
    ];
    Exp.fun_("", None, Pat.var(Location.mknoloc("value")), body);
  }
};
