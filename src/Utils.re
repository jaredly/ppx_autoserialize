/* open Migrate_parsetree.Ast_403; */

let fail = (loc, txt) => raise(Location.Error(Location.error(~loc=loc, txt)));

let left = (txt) => Ast_helper.Pat.var(Location.mknoloc(txt));

let simple = (txt) => Ast_helper.Exp.ident(Location.mknoloc(txt));

let strConst = (txt) => Ast_helper.Exp.constant(Const_string(txt, None));

let patVar = (txt) => Ast_helper.Pat.var(Location.mknoloc(txt));

let expIdent = (txt) => Ast_helper.Exp.ident(Location.mknoloc(Longident.Lident(txt)));

let rec suffixify = (lident, suffix) =>
  Longident.(
    switch lident {
    | Lident(x) => Lident(x ++ suffix)
    | Ldot(x, y) => Ldot(x, y ++ suffix)
    | Lapply(a, b) => Lapply(a, suffixify(b, suffix))
    }
  );

let rec patList = (items: list(Parsetree.pattern)) =>
  List.fold_right(
    (item, rest) =>
      Ast_helper.Pat.construct(
        Location.mknoloc(Longident.Lident("::")),
        Some(Ast_helper.Pat.tuple([item, rest]))
      ),
    items,
    Ast_helper.Pat.construct(Location.mknoloc(Longident.Lident("[]")), None)
  );

let rec list = (items: list(Parsetree.expression)) =>
  List.fold_right(
    (item, rest) =>
      Ast_helper.Exp.construct(
        Location.mknoloc(Longident.Lident("::")),
        Some(Ast_helper.Exp.tuple([item, rest]))
      ),
    items,
    Ast_helper.Exp.construct(Location.mknoloc(Longident.Lident("[]")), None)
  );

let rec chainExpressions = (expressions) =>
  switch expressions {
  | [] => assert false
  | [one] => one
  | [one, ...rest] => Ast_helper.Exp.sequence(one, chainExpressions(rest))
  };

let paramd_fun = (param_names, body) =>
  Parsetree.(
    Longident.(
      Ast_helper.(
        List.fold_right(
          (name, body) =>
            Exp.fun_(
              "",
              None,
              Pat.var(Location.mknoloc(name ++ "_converter")),
              body
            ),
          param_names,
          body
        )
      )
    )
  );

let paramd_type = (param_names, body, typ) =>
  Parsetree.(
    Longident.(
      Ast_helper.(
        List.fold_right(
          (name, body) => {
            let vtyp = Ast_helper.Typ.var(name);
            open SharedTypes;
            let converter =
              switch typ {
              | To(typ) => Ast_helper.Typ.arrow("", vtyp, typ)
              | From(typ) => Ast_helper.Typ.arrow("", typ, [%type : option([%t vtyp])])
              };
            Typ.arrow("", converter, body);
          },
          param_names,
          body
        )
      )
    )
  );
