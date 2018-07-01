let fixtures = [
  (
    /* (input, output) */
    [%str type x = int],
    [%str type x = int; let x__from_json = (value) => int__from_json(value)]
  ),
  (
    [%str
      type x = {
        a: int,
        c: string
      }
    ],
    [%str
      type x = {
        a: int,
        c: string
      };
      let x__from_json = (value) =>
        switch (Js.Json.classify(value)) {
        | Js.Json.JSONObject(value) =>
          switch (Js.Dict.get(value, "a")) {
          | None => None
          | Some((attr)) =>
            switch (int__from_json(attr)) {
            | None => None
            | Some((a_extracted)) =>
              switch (Js.Dict.get(value, "c")) {
              | None => None
              | Some((attr)) =>
                switch (string__from_json(attr)) {
                | None => None
                | Some((c_extracted)) => Some({a: a_extracted, c: c_extracted})
                }
              }
            }
          }
        | _ => None
        }
    ]
  ),
  (
    [%str module type T = {type t;}],
    [%str module type T = {type t; let t__from_json: Js.Json.t => option(t);}]
  )
];
