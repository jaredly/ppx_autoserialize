let unit__to_json () = Js.Json.string ""
let int__to_json x = Js.Json.number (float_of_int x)
let float__to_json = Js.Json.number
let list__to_json convert items =
  Js.Json.array (Array.of_list (List.map convert items))
let string__to_json = Js.Json.string
let array__to_json convert items = Js.Json.array (Array.map convert items)
let bool__to_json b = Js.Json.boolean @@ (Js.Boolean.to_js_boolean b)
let option__to_json convert value =
  match value with
  | None  -> Js.Json.null
  | ((Some (value))[@explicit_arity ]) -> Js.Json.array [|(convert value)|]
let unit__from_json _ = ((Js.Result.Ok (()))[@explicit_arity ])
let int__from_json x =
  match Js.Json.classify x with
  | ((Js.Json.JSONNumber (n))[@explicit_arity ]) ->
      ((Js.Result.Ok ((int_of_float n)))[@explicit_arity ])
  | _ -> ((Js.Result.Error (None))[@explicit_arity ])
let float__from_json x =
  match Js.Json.classify x with
  | ((Js.Json.JSONNumber (n))[@explicit_arity ]) ->
      ((Js.Result.Ok (n))[@explicit_arity ])
  | _ -> ((Js.Result.Error (None))[@explicit_arity ])
let list__from_json convert items =
  match Js.Json.classify items with
  | ((Js.Json.JSONArray (arr))[@explicit_arity ]) ->
      (try
         let items =
           Array.map
             (fun item  ->
                match convert item with
                | ((Some (x))[@explicit_arity ]) -> x
                | None  -> failwith "Item failed to parse") arr in
         ((Js.Result.Ok ((Array.to_list items)))[@explicit_arity ])
       with | _ -> ((Js.Result.Error (None))[@explicit_arity ]))
  | _ -> ((Js.Result.Error (None))[@explicit_arity ])
let string__from_json value =
  Js.log2 "str" value;
  (match Js.Json.classify value with
   | ((Js.Json.JSONString (str))[@explicit_arity ]) ->
       ((Js.Result.Ok (str))[@explicit_arity ])
   | _ -> ((Js.Result.Error (None))[@explicit_arity ]))
let array__from_json convert items =
  match Js.Json.classify items with
  | ((Js.Json.JSONArray (arr))[@explicit_arity ]) ->
      (try
         let items =
           Array.map
             (fun item  ->
                match convert item with
                | ((Some (x))[@explicit_arity ]) -> x
                | None  -> failwith "Item failed to parse") arr in
         ((Js.Result.Ok (items))[@explicit_arity ])
       with | _ -> ((Js.Result.Error (None))[@explicit_arity ]))
  | _ -> ((Js.Result.Error (None))[@explicit_arity ])
let bool__from_json value =
  match Js.Json.classify value with
  | Js.Json.JSONFalse  -> ((Js.Result.Ok (false))[@explicit_arity ])
  | Js.Json.JSONTrue  -> ((Js.Result.Ok (true))[@explicit_arity ])
  | _ -> ((Js.Result.Error (None))[@explicit_arity ])
let option__from_json convert value =
  match Js.Json.classify value with
  | Js.Json.JSONNull  -> ((Js.Result.Ok (None))[@explicit_arity ])
  | ((Js.Json.JSONArray ([|item|]))[@explicit_arity ]) ->
      (match convert item with
       | ((Js.Result.Error (v))[@explicit_arity ]) ->
           ((Js.Result.Error (v))[@explicit_arity ])
       | ((Js.Result.Ok (value))[@explicit_arity ]) ->
           ((Js.Result.Ok ((Some value)))[@explicit_arity ]))
  | _ -> ((Js.Result.Error (None))[@explicit_arity ])
external to_devtools : 'a -> <  > Js.t = "%identity"
let unit__to_devtools = to_devtools
let int__to_devtools = to_devtools
let float__to_devtools = to_devtools
let string__to_devtools = to_devtools
let bool__to_devtools = to_devtools
let list__to_devtools convert items =
  ([%bs.obj
     { ($bs) = "list"; items = ((List.map convert items) |> Array.of_list) }])
    |> to_devtools
let array__to_devtools convert items =
  (Array.map convert items) |> to_devtools
let option__to_devtools convert item =
  (match item with
   | None  ->
       [%bs.obj { ($bs) = "optional"; empty = true; value = Js.Null.empty }]
   | ((Some (x))[@explicit_arity ]) ->
       [%bs.obj
         {
           ($bs) = "optional";
           empty = false;
           value = (Js.Null.return (convert x))
         }])
    |> to_devtools
type derp = {
  blah: string option;}
let derp__to_json value =
  let result = Js.Dict.empty () in
  Js.Dict.set result "blah" ((option__to_json string__to_json) value.blah);
  Js.Json.object_ result
let derp__from_json value =
  match Js.Json.classify value with
  | ((Js.Json.JSONObject (value))[@explicit_arity ]) ->
      (match (option__from_json string__from_json)
               (match Js.Dict.get value "blah" with
                | None  -> Js.Json.null
                | ((Some (attr))[@explicit_arity ]) -> attr)
       with
       | ((Js.Result.Error
           (((Some (key))[@explicit_arity ])))[@explicit_arity ]) ->
           ((Js.Result.Error ((Some ("blah" ^ ("." ^ key)))))[@explicit_arity
                                                               ])
       | Js.Result.Error _ ->
           ((Js.Result.Error ((Some "blah")))[@explicit_arity ])
       | ((Js.Result.Ok (blah_extracted))[@explicit_arity ]) ->
           ((Js.Result.Ok ({ blah = blah_extracted }))[@explicit_arity ]))
  | _ -> ((Js.Result.Error (None))[@explicit_arity ])
let derp__to_devtools value =
  let result = Js.Dict.empty () in
  Js.Dict.set result "blah"
    ((option__to_devtools string__to_devtools) value.blah);
  ([%bs.obj
     { ($bs) = "record"; type = "derp"; attributes = (result |> to_devtools)
     }])
    |> to_devtools
