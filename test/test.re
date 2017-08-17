
type my_int = int;
type string_list = list string;
type nested = {name: string};
type jumbo = {
  str: string,
  num: int,
  float: float,
  my_int: my_int,
  string_list: string_list,
  nums: array int,
  nested: nested,
};

let check fromfn tofn value => {
  let str = Yojson.Safe.to_string (tofn value);
  /* print_endline str; */
  switch (fromfn (Yojson.Safe.from_string str)) {
    | None => {
      (1, 1)
    }
    | Some res => {
      if (res == value) {
        (1, 0)
      } else {
        print_endline "Got back a different thing";
        (1, 1)
      }
    }
  }
};

let run () => {
  let (total, failed) = List.fold_left
  (fun (total, failed) (t, f) => (total + t, failed + f))
  (0, 0)
  [
    check my_int__from_yojson my_int__to_yojson 20,
    check string_list__from_yojson string_list__to_yojson ["a", "b"],
    check nested__from_yojson nested__to_yojson {name: "awesome"},
    check
    jumbo__from_yojson
    jumbo__to_yojson
    {
      str: "str",
      num: 10,
      float: 5.2,
      my_int: 3,
      string_list: ["one", "two", "threee"],
      nums: [|1,2,3|],
      nested: {name: "Me"},
    },
  ];

  if (failed == 0) {
    Printf.printf "All %d passed\n" total;
  } else {
    Printf.printf "Total: %d; Failed: %d\n" total failed;
  }
};

run();
