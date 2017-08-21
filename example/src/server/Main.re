let module CoServer = Cohttp_lwt_unix.Server;
open Shared.Types;

type appState = {
  nextId: int,
  todos: todos,
};

let appState = ref {
  nextId: 1,
  todos: [{
    id: 0,
    text: "Add some things to do",
    completed: None,
  }]
};

let maybe_parse text => try(Some (Yojson.Safe.from_string text)) {
| _ => None
};

let withBody body fn => Lwt.(Cohttp_lwt_body.to_string body >>= fn);
let withJsonBody body fn => withBody body (fun body => fn (maybe_parse body));

Server.get_prefix "/" (Server.serveStatic "../public");
Server.get "/todos" (fun _ _ _ => Server.json (todos__to_yojson (!appState).todos));
Server.post "/todo/add" (fun _ body _ => withBody body (fun body => {
  let id = (!appState).nextId;
  let todo = {completed: None, id, text: body};
  appState := {
    nextId: id + 1,
    todos: [todo, ...(!appState).todos],
  };
  Server.json (todo__to_yojson todo)
}));

Server.post "/todo" Lwt.(fun _ body _ => Cohttp_lwt_body.to_string body >>= (fun body => {
  switch (maybe_parse body) {
  | None => CoServer.respond_string status::`Bad_request body::"" ()
  | Some data => switch (todo__from_yojson data) {
    | None => CoServer.respond_string status::`Bad_request body::"" ()
    | Some data => {
      let m = ref 2;
      m := 3;
      appState := {
        ...!appState,
        todos: List.map (fun item => item.id === data.id ? data : item) (!appState).todos,
      };
      CoServer.respond_string status::`No_content body::"" ()
    }
    }
  }
}));

/* fallback */
Server.get_prefix "/" (fun _ _ _ _ => {
  CoServer.respond_string status::`Not_found body::"Welcome to never land" ()
});

Server.listen 8000;
