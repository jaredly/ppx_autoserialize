/* open Migrate_parsetree.Ast_403; */

type tofrom =
  | To(Parsetree.core_type)
  | From(Parsetree.core_type);

type config = {
  prefix: Parsetree.structure,
  suffix: string,
  typ: tofrom,
  variant:
    (
      Parsetree.core_type => Parsetree.expression,
      list(Parsetree.constructor_declaration),
      string
    ) =>
    Parsetree.expression,
  record:
    (Parsetree.core_type => Parsetree.expression, list(Parsetree.label_declaration), string) =>
    Parsetree.expression
};
