/* open Migrate_parsetree.Ast_403; */

/* module Convert =
  Migrate_parsetree.Convert(Migrate_parsetree.OCaml_403, Migrate_parsetree.OCaml_current); */

let show_structure = (structure) => {
  Pprintast.structure(Format.str_formatter,
  /* Convert.copy_structure */
  (structure));
  Format.flush_str_formatter();
};

let fixtures = [
  (JsonStringify.fixtures, Lib.Json.stringify),
  (DevtoolsSerialize.fixtures, Lib.Devtools.config),
  (JsonParse.fixtures, Lib.Json.parse),
  (YojsonStringify.fixtures, Lib.YoJson.stringify)
];

let show_error = (input, result, expected) => {
  print_endline(">> Input:");
  print_endline(show_structure(input));
  print_endline(">> Output:");
  print_endline(show_structure(result));
  print_endline(">> Expected:");
  print_endline(show_structure(expected));
};

let run = () => {
  let (total, failures) =
    List.fold_left(
      ((total, failures), (fixtures, config)) => {
        let mapper = Lib.mapper([{...config, prefix: []}]);
        List.fold_left(
          ((total, failures), (input, expected)) =>
            try {
              let result = mapper.structure(mapper, input);
              if (result != expected) {
                show_error(input, result, expected);
                (total + 1, failures + 1);
              } else {
                (total + 1, failures);
              };
            } {
            | Location.Error(error) =>
              print_endline(">> Input:");
              print_endline(show_structure(input));
              print_endline(">> Error:");
              print_endline(error.Location.msg);
              (total + 1, failures + 1);
            },
          (total, failures),
          fixtures
        );
      },
      (0, 0),
      fixtures
    );
  if (failures !== 0) {
    Printf.printf("Total: %d, Failures: %d", total, failures);
    exit(1);
  } else {
    Printf.printf("All %d succeeded!", total);
    exit(0);
  };
};

run();
