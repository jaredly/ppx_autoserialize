
let fixtures = [(/* (input, output) */
  [%str type x = int],
  [%str
    type x = int;
    let x__'bs'from_json value => int__'bs'from_json value;
  ]
), (
  [%str type x = {a: int, c: string}],
  [%str
    type x = {a: int, c: string};
    let x__'bs'from_json = fun value => {
      switch (Js.Json.classify value) {
      | JSONObject value => {
        switch (Js.Dict.get value "a") {
        | None => None
        | Some attr => switch (int__'bs'from_json attr) {
          | None => None
          | Some a_extracted => switch (Js.Dict.get value "c") {
            | None => None
            | Some attr => switch (string__'bs'from_json attr) {
              | None => None
              | Some c_extracted => {
                Some {a: a_extracted, c: c_extracted}
              }
              }
            }
          }
        }
      }
      | _ => None
      }
    };
  ]
)];
