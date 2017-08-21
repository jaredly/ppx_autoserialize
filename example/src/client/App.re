
open! Types;

module type Config = {
  type data;
  let url: string;
  let data__from_json: Js.Json.t => option data;
  let data__to_json: data => Js.Json.t;
  let data__to_devtools: data => Js.t {.};
  let make: data::data => array int =>  ReasonReact.componentSpec ReasonReact.stateless ReasonReact.stateless ReasonReact.noRetainedProps ReasonReact.noRetainedProps;
};

let fetchJson: string => (Js.Json.t => unit) => unit = [%bs.raw {| function (url, fn) {
  fetch(url).then(res => res.json()).then(fn)
} |}];

let str = ReasonReact.stringToElement;

let module Loader (Config: Config) => {
  type state = Loading | Error string | Loaded Config.data;
  let component = ReasonReact.statefulComponent "Loader";
  let make _children => {
    ...component,
    initialState: fun () => Loading,
    didMount: fun {update} => {
      fetchJson Config.url (update (fun json _ => switch (Config.data__from_json json) {
      | None => ReasonReact.NoUpdate
      | Some data => ReasonReact.Update (Loaded data)
      }));
      ReasonReact.NoUpdate
    },
    render: fun self => {
      switch self.state {
      | Loading => <div>(str "Loading")</div>
      | Error text => <div>(str "Error") (str text)</div>
      | Loaded data => <Config data />
      }
    }
  };
};

let style = ReactDOMRe.Style.make;

let module TodoItem = {
  let component = ReasonReact.statelessComponent "Todo";
  let make ::item _ => {
    ...component,
    render: fun _ => <div style=(style flexDirection::"row" padding::"10px" ())>
      <input _type="checkbox" checked=(item.completed !== None |> Js.Boolean.to_js_boolean) />
      <div style=(style flexBasis::"10px" ()) />
      <div>
        (str item.text)
      </div>
    </div>
  }
};

let module Todos = {
  let component = ReasonReact.statelessComponent "Todos";
  let url = "/todos";
  type data = todos;
  let make ::data _ => {
    ...component,
    render: fun _ => {
      <div>
      <pre>
        (str (Js.Json.stringify (todos__to_json data)))
      </pre>
      (List.map
      (fun item => <div>
        (str (Js.Json.stringify (todo__to_json item)))
        <TodoItem item />
      </div>)
      data |> Array.of_list |> ReasonReact.arrayToElement)
      </div>
    }
  }
};

let module LoadedTodos = Loader Todos;

let module Page = {
  let component = ReasonReact.statelessComponent "Page";
  let make _children => {
    ...component,
    render: fun _ =>
      <div>
        (str "A Nice Todo List")
        <LoadedTodos />
      </div>
  };
};

ReactDOMRe.renderToElementWithId <Page /> "index";

Js.log "Hello, BuckleScript and Reason!";

type sum = | One int | Two float string | Three;
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
  cross_module: Lib.person,
  sum: list (option sum),
};

Devtools.register();

let value = {
  str: "str",
  num: 10,
  float: 5.2,
  my_int: 3,
  string_list: ["one", "two", "threee"],
  nums: [|1,2,3|],
  nested: {name: "Me"},
  cross_module: {Lib.name: "Reason"},
  sum: [None, None, Some Three, Some (One 2), Some (Two 4.5 "hi")],
};

Js.log (jumbo__to_devtools value);

Js.log (Js.Json.stringify (int__to_json 10));

let ss = Js.Json.stringify (jumbo__to_json value);
Js.log ss;