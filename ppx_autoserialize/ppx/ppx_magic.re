open Migrate_parsetree.Ast_403;

let my_rewriter config cookies =>
  Lib.mapper [
    Lib.YoJson.parse,
    Lib.YoJson.stringify
  ];

let () =
  Migrate_parsetree.Driver.register name::"ppx_magic"
    Migrate_parsetree.Versions.ocaml_403 my_rewriter;

/* let () = Migrate_parsetree.Driver.run_as_ppx_rewriter(); */
