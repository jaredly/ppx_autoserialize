/* open Migrate_parsetree.Ast_403; */

let () = Ast_mapper.run_main(args => Lib.mapper([Lib.Json.stringify, Lib.Json.parse, Lib.Devtools.config]));

/* let () =
  Migrate_parsetree.Driver.register(
    ~name="ppx_magic",
    Migrate_parsetree.Versions.ocaml_403,
    my_rewriter
  );

let () = Migrate_parsetree.Driver.run_as_ppx_rewriter(); */
