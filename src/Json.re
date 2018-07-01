open SharedTypes;

open Utils;

open Migrate_parsetree.Ast_403;

let stringify = {
  prefix: [%str
    let unit__to_json = () => Js.Json.string("");
    let int__to_json = (x) => Js.Json.number(float_of_int(x));
    let float__to_json = Js.Json.number;
    let list__to_json = (convert, items) => Js.Json.array(Array.of_list(List.map(convert, items)));
    let string__to_json = Js.Json.string;
    let array__to_json = (convert, items) => Js.Json.array(Array.map(convert, items));
    let bool__to_json = (b) => Js.Json.boolean @@ Js.Boolean.to_js_boolean(b);
    let option__to_json = (convert, value) =>
      switch value {
      | None => Js.Json.null
      | Some((value)) => Js.Json.array([|convert(value)|])
      }
  ],
  suffix: "__to_json",
  typ: To([%type : Js.Json.t]),
  variant: (core_type_converter, constructors, name) => {
    open Parsetree;
    open Ast_helper;
    open Longident;
    let cases =
      List.map(
        ({pcd_name: {txt, loc}, pcd_args}) => {
          let strConst = Exp.constant(Pconst_string(txt, None));
          let lid = Location.mknoloc(Lident(txt));
          switch pcd_args {
          | Pcstr_tuple(types) =>
            switch types {
            | [] => Exp.case(Pat.construct(lid, None), [%expr Js.Json.string([%e strConst])])
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
              let values = [[%expr Js.Json.string([%e strConst])], ...values];
              Exp.case(pat, [%expr Js.Json.array([%e Exp.array(values)])]);
            } /* This isn't supported in 4.02 anyway */
          | Pcstr_record(labels) => Utils.fail(loc, "Nope record labels")
          };
        },
        constructors
      );
    Exp.fun_(Asttypes.Nolabel, None, Utils.patVar("value"), Exp.match([%expr value], cases));
  },
  record: (core_type_converter, labels, name) => {
    open Parsetree;
    open Longident;
    open Ast_helper;
    let sets =
      List.map(
        ({pld_name: {txt}, pld_type}) => {
          let value = Exp.field([%expr value], Location.mknoloc(Lident(txt)));
          let strConst = Exp.constant(Pconst_string(txt, None));
          [%expr
            Js.Dict.set(result, [%e strConst], [%e core_type_converter(pld_type)]([%e value]))
          ];
        },
        labels
      );
    let body = List.append(sets, [[%expr Js.Json.object_(result)]]) |> chainExpressions;
    let body =
      Exp.let_(Nonrecursive, [Ast_helper.Vb.mk(left("result"), [%expr Js.Dict.empty()])], body);
    Exp.fun_(Asttypes.Nolabel, None, Pat.var(Location.mknoloc("value")), body);
  }
};

let parse = {
  prefix: [%str
    let unit__from_json = (_) => Some();
    let int__from_json = (x) =>
      switch (Js.Json.classify(x)) {
      | Js.Json.JSONNumber(n) => Some(int_of_float(n))
      | _ => None
      };
    let float__from_json = (x) =>
      switch (Js.Json.classify(x)) {
      | Js.Json.JSONNumber(n) => Some(n)
      | _ => None
      };
    let list__from_json = (convert, items) =>
      switch (Js.Json.classify(items)) {
      | Js.Json.JSONArray(arr) =>
        try {
          let items =
            Array.map(
              (item) =>
                switch (convert(item)) {
                | Some((x)) => x
                | None => failwith("Item failed to parse")
                },
              arr
            );
          Some(Array.to_list(items));
        } {
        | _ => None
        }
      | _ => None
      };
    let string__from_json = (value) =>
      switch (Js.Json.classify(value)) {
      | Js.Json.JSONString(str) => Some(str)
      | _ => None
      };
    let array__from_json = (convert, items) =>
      switch (Js.Json.classify(items)) {
      | Js.Json.JSONArray(arr) =>
        try {
          let items =
            Array.map(
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
    let bool__from_json = (value) =>
      switch (Js.Json.classify(value)) {
      | Js.Json.JSONFalse => Some(false)
      | Js.Json.JSONTrue => Some(true)
      | _ => None
      };
    let option__from_json = (convert, value) =>
      switch (Js.Json.classify(value)) {
      | Js.Json.JSONNull => Some(None)
      | Js.Json.JSONArray([|item|]) =>
        switch (convert(item)) {
        | None => None
        | Some((value)) => Some(Some(value))
        }
      | _ => None
      }
  ],
  suffix: "__from_json",
  typ: From([%type : Js.Json.t]),
  variant: (core_type_converter, constructors, name) => {
    /* [%expr fun _ => failwith "not supported"] */
    open Parsetree;
    open Ast_helper;
    open
      Longident; /* [%expr
    fun value => {
      switch (Js.Json.classify value) {
      | JSONString "awesome" =>
      | JSONArray arr when Js.Json.classify arr.(0) == JSONString "moresome" => {

      }
      }
    }
    ] */
    let cases =
      List.map(
        ({pcd_name: {txt, loc}, pcd_args}) => {
          let patConst =
            Pat.constant(
              Pconst_string(txt, None)
            ); /* let processArgs =
      let body = [%expr switch items {
      | [%p Pat.array [Pat.any (), ...args]] => {
        [%e processArgs]
      }
      | _ => None
      }]; */
          let strConst = Exp.constant(Pconst_string(txt, None));
          let lid = Location.mknoloc(Lident(txt));
          switch pcd_args {
          | Pcstr_tuple(types) =>
            switch types {
            | [] =>
              Exp.case(
                [%pat ? Js.Json.JSONString([%p patConst])],
                [%expr Some([%e Exp.construct(lid, None)])]
              )
            | _ =>
              let items = List.mapi((i, typ) => Utils.patVar("arg" ++ string_of_int(i)), types);
              let pattern = Pat.array([Pat.any(), ...items]);
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
              Exp.case(
                [%pat ? Js.Json.JSONArray(arr)],
                ~guard=[%expr Js.Json.classify(arr[0]) == Js.Json.JSONString([%e strConst])],
                [%expr
                  switch arr {
                  | [%p pattern] => [%e body]
                  | _ => None
                  }
                ]
              );
            } /* This isn't supported in 4.02 anyway */
          | Pcstr_record(labels) => Utils.fail(loc, "Nope record labels")
          };
        },
        constructors
      );
    let cases = List.append(cases, [Exp.case(Pat.any(), [%expr None])]);
    Exp.fun_(
      Asttypes.Nolabel,
      None,
      Utils.patVar("value"),
      Exp.match([%expr Js.Json.classify(value)], cases)
    );
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
          let strConst = Exp.constant(Pconst_string(txt, None));
          let strPat = Pat.var(Location.mknoloc(txt ++ "_extracted"));
          [%expr
            switch (Js.Dict.get(value, [%e strConst])) {
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
      switch (Js.Json.classify(value)) {
      | Js.Json.JSONObject(value) => [%e body]
      | _ => None
      }
    ];
    Exp.fun_(Asttypes.Nolabel, None, Pat.var(Location.mknoloc("value")), body);
  }
};
