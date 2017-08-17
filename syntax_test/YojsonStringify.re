
let fixtures = [(
  [%str type x = One string | Two int float | Three],
  [%str type x = One string | Two int float | Three;
  let x__to_yojson value => switch value {
  | One (arg0) => `List [`String "One", string__to_yojson arg0]
  | Two arg0 arg1 => `List [`String "Two", int__to_yojson arg0, float__to_yojson arg1]
  | Three => `String "Three"
  }
  ],
)];
