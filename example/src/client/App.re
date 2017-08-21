
open! Types;

let str = ReasonReact.stringToElement;
let style = ReactDOMRe.Style.make;

let module TodoItem = {
  let component = ReasonReact.statelessComponent "Todo";
  let make ::item ::onToggle ::onEdit _ => {
    ...component,
    render: fun _ => <div
      className=(Glamor.(css [
        backgroundColor "white",
        cursor "pointer",
        flexDirection "row",
        padding "10px",
        Selector ":hover" [
          backgroundColor "#eee"
        ]
      ]))
      onClick=onToggle
    >
      <input _type="checkbox" checked=(item.completed !== None |> Js.Boolean.to_js_boolean) />
      <div style=(style flexBasis::"10px" ()) />
      <Editor value=(item.text) onChange=onEdit />
    </div>
  }
};

let jsNow: unit => int = [%bs.raw "function() {return Date.now()}"];

let post: string => Js.Json.t => (Js.Json.t => unit) => unit = [%bs.raw {|
  function (path, json, onDone) {
    fetch(path, {body: JSON.stringify(json), method: 'POST', headers: {'Content-Type': 'application/json'}})
    .then(res => res.json())
    .then(res => onDone(res), err => console.error('failed', err))
  }
|}];

let updateItem item onDone => {
  post "/todo" (todo__to_json item) (fun data => {
    switch (todos__from_json data) {
    | None => Js.log "Failed to parse data"
    | Some todos => {
      Js.log2 "New todos" (todos__to_devtools todos);
      onDone todos
    }
    }
  })
};

let module Todos = {
  let component = ReasonReact.statefulComponent "Todos";
  let url = "/todos";
  type state = todos;
  type data = todos;
  let toggleItem item update => {
    let item = item.completed === None
      ? {...item, completed: Some (jsNow())}
      : {...item, completed: None};
    updateItem item update
  };
  let editItem item text update => text !== item.text ? updateItem {...item, text} update : ();
  let make ::data _ => {
    ...component,
    initialState: fun () => data,
    render: fun {state, update} => {
      let updateTodos = (update (fun todos _ => ReasonReact.Update todos));
      <div>
      (List.map
      (fun item => 
        <TodoItem
          item
          key=(item.id |> string_of_int)
          onToggle=(fun _ => toggleItem item updateTodos)
          onEdit=(fun text => editItem item text updateTodos)
        />
      )
      state |> Array.of_list |> ReasonReact.arrayToElement)
      </div>
    }
  }
};

let module LoadedTodos = Loader.F Todos;

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

Devtools.register();
ReactDOMRe.renderToElementWithId <Page /> "index";
