
let fixtures = [(/* (input, output) */
  [%str type x = int],
  [%str
    type x = int;
    let x__'bs'to_json value => int__'bs'to_json value;
  ]
), (
  [%str type x = awesome],
  [%str
    type x = awesome;
    let x__'bs'to_json value => awesome__'bs'to_json value;
  ]
), (
  [%str type x = list int],
  [%str
    type x = list int;
    let x__'bs'to_json value => (list__'bs'to_json int__'bs'to_json) value;
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
    let x__'bs'to_json a_converter value => (list__'bs'to_json a_converter) value
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
