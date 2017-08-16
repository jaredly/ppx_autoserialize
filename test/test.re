
open Migrate_parsetree.Ast_403;

let module Convert = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_403)(Migrate_parsetree.OCaml_current);

let show_structure structure => {
  Pprintast.structure Format.str_formatter (Convert.copy_structure structure);
  Format.flush_str_formatter();
};

let json_fixtures = [(/* (input, output) */
  [%str type x = int],
  [%str
    type x = int;
    let x__'bs'to_json = int__'bs'to_json;
  ]
), (
  [%str type x = awesome],
  [%str
    type x = awesome;
    let x__'bs'to_json = awesome__'bs'to_json;
  ]
), (
  [%str type x = list int],
  [%str
    type x = list int;
    let x__'bs'to_json = list__'bs'to_json int__'bs'to_json;
  ]
), (
  [%str type x = {a: int, c: string}],
  [%str
    type x = {a: int, c: string};
    let x__'bs'to_json = fun value => {
      let result = Js.Dict.empty ();
      Js.Dict.set result "a" (int__'bs'to_json value.a);
      Js.Dict.set result "c" (string__'bs'to_json value.c);
      Js.Json.object_ result
    };
  ]
), (
  [%str type x 'a = list 'a],
  [%str
    type x 'a = list 'a;
    let x__'bs'to_json a_converter => list__'bs'to_json a_converter
  ]
), (
  [%str type x 'a = {a: 'a, c: string}],
  [%str
    type x 'a = {a: 'a, c: string};
    let x__'bs'to_json = fun a_converter value => {
      let result = Js.Dict.empty ();
      Js.Dict.set result "a" (a_converter value.a);
      Js.Dict.set result "c" (string__'bs'to_json value.c);
      Js.Json.object_ result
    };
  ]
  /** TODO records, variants. maybe that's it? */
)];

/* Not implemented yet */
let todo = [(
  [%str [%%import One [@as One']][@@from Three]],
  [%str [%%import two [@as two']][@@from Three]],
  [%str [%%import (One [@as One'], two [@as two'])][@@from Three]],
  [%str [%import ExpressionLevel][@@from Somewhere]],
)];

let run () => {
  let mapper = Lib.mapper [{
    ...Lib.jsonConfig,
    prefix: []
  }];
  let (total, failures) = List.fold_left (fun (total, failures) (input, expected) => {
    try {
    let result = mapper.structure mapper input;
    if (result != expected) {
      print_endline ">> Input:";
      print_endline (show_structure input);
      print_endline ">> Output:";
      print_endline (show_structure result);
      print_endline ">> Expected:";
      print_endline (show_structure expected);
      (total + 1, failures + 1)
    } else {
      (total + 1, failures)
    }
    } {
      | Location.Error error => {
        print_endline ">> Input:";
        print_endline (show_structure input);
        print_endline ">> Error:";
        print_endline error.Location.msg;
        (total + 1, failures + 1)
      }
    }
  }) (0, 0) json_fixtures;

  if (failures !== 0) {
      Printf.printf "Total: %d, Failures: %d" total failures;
    exit 1;
  } else {
    Printf.printf "All %d succeeded!" total;
    exit 0;
  }
};

run ();
