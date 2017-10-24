open Jest;
open Js.Result;

type subRecord = {
  v: string
};

type variant =
  | One string
  | Two string string
  | Three string string string;

type record = {
  s: string,
  i: int,
  b: bool,
  u: unit,
  o: option string,
  a: array int,
  l: list float,
  v: variant,
  sub: option subRecord
};

type strArr = array string;

describe "__from_json" (fun () => {
  open Expect;

  describe "Ok" (fun () => {
    test "record" (fun () => {
      let src = {
        s: "test",
        i: 9,
        b: true,
        u: (),
        o: Some "abc",
        a: [|1, 2, 3|],
        l: [4.0, 4.1, 4.2],
        v: Two "one" "two",
        sub: Some { v: "ok" }
      };

      let result = src |> record__to_json |> record__from_json;
      switch result {
        | Ok r => expect r |> toEqual src
        | Error e => failwith ("Failed with " ^ (Js.String.make e))
      };
    });
  });

  describe "Error" (fun () => {
    test "record" (fun () => {
      let jsonStr = "{\"s\":\"test\",\"i\":9,\"b\":3,\"u\":\"\",\"o\":[\"abc\"],\"a\":[1,2,3],\"l\":[4,4.1,4.2],\"v\":[\"Two\",\"one\",\"two\"],\"sub\":[{\"v\":\"ok\"}]}";
      let result = jsonStr |> Js.Json.parseExn |> record__from_json;
      switch result {
        | Ok _ => failwith "Shouldn't have parsed"
        | Error None => failwith "No key given"
        | Error (Some key) => expect key |> toEqual "b"
      };
    });

    test "array" (fun () => {
      let jsonStr = "[1, 2, 3]";
      let result = jsonStr |> Js.Json.parseExn |> strArr__from_json;
      switch result {
        | Ok _ => fail "Shouldn't have parsed"
        | Error (Some key) => fail ("No key expected, given: " ^ key)
        | Error None => pass
      };
    });

    test "list" (fun () => {
      let jsonStr = "[1, 2, 3]";
      let result = jsonStr |> Js.Json.parseExn |> strArr__from_json;
      switch result {
        | Ok _ => fail "Shouldn't have parsed"
        | Error (Some key) => fail ("No key expected, given: " ^ key)
        | Error None => pass
      };
    });

    test "variant" (fun () => {
      let jsonStr = "[\"Two\",\"ok\",0]";
      let result = jsonStr |> Js.Json.parseExn |> variant__from_json;
      switch result {
        | Ok _ => failwith "Shouldn't have parsed"
        | Error None => failwith "No key given"
        | Error (Some key) => expect key |> toEqual "arg1"
      };
    });
  });
});