
let str = ReasonReact.stringToElement;
let style = ReactDOMRe.Style.make;
let evtValue event => (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value;

let component = ReasonReact.statefulComponent "Editor";
type state = option string;
let make ::value ::onChange _ => {
  ...component,
  initialState: fun () => None,
  willReceiveProps: fun {state} => switch state {
  | None => None
  | Some text => text === value ? None : Some text
  },
  render: fun {state, update} => {
    switch state {
    | None => <div
        style=(style cursor::"text" ())
        onClick=(fun evt => {
          ReactEventRe.Mouse.stopPropagation evt;
          (update (fun _ _ => ReasonReact.Update (Some value))) evt;
          })
      >(str value)</div>
    | Some text => <input
        value=text
        autoFocus=(Js.Boolean.to_js_boolean true)
        onChange=(update (fun evt _ => ReasonReact.Update (Some (evtValue evt))))
        onClick=(fun evt => ReactEventRe.Mouse.stopPropagation evt)
        onKeyDown=(fun evt => {
          switch (ReactEventRe.Keyboard.key evt) {
          | "Enter" => if (text == value) {
            (update (fun _ _ => ReasonReact.Update None)) ()
          } else {
            onChange text
          }
          | _ => ()
          }
        })
        style=(style fontFamily::"inherit" flex::"1" fontSize::"inherit" ())
        onBlur=(fun _ => {
          if (text != value) {
            onChange text /* TODO I want a different state value that is "waiting" */
          } else {
            (update (fun _ _ => ReasonReact.Update None)) ()
          }
        })
      />
    }
  }
};
