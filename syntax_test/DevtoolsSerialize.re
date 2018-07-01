let fixtures = [
  (
    /* (input, output) */
    [%str type x = int],
    [%str type x = int; let x__to_devtools = (value) => int__to_devtools(value)]
  ),
  (
    [%str
      type x =
        | One
        | Two
        | Three
    ],
    [%str
      type x =
        | One
        | Two
        | Three;
      let x__to_devtools = (value) =>
        switch value {
        | One =>
          {"$bs": "variant", "type": "x", "constructor": "One", "arguments": [||]} |> to_devtools
        | Two =>
          {"$bs": "variant", "type": "x", "constructor": "Two", "arguments": [||]} |> to_devtools
        | Three =>
          {"$bs": "variant", "type": "x", "constructor": "Three", "arguments": [||]} |> to_devtools
        }
    ]
  )
];