
open Lwt
open V1_LWT

let make_tracer () =
  let traces = ref [] in
  let trace sexp = (traces := sexp :: !traces)
  and get ()     = List.rev !traces in
  (trace, get)

let rec map_partial ~f = function
  | []    -> []
  | x::xs -> match f x with
      | None   ->      map_partial ~f xs
      | Some y -> y :: map_partial ~f xs

let bytes_dump sexp : Yojson.json =
  let open Sexplib.Sexp in
  match sexp with
  | List bs ->
      let repr =
        List.map (fun (List bytes) ->
          `String (List.map (fun (Atom x) -> x) bytes |> String.concat " "))
        bs in
      `List repr

open Sexplib.Sexp

let rec flatten_sexp = function
  | Atom x  -> x
  | List xs -> List.map flatten_sexp xs |> String.concat " "

let dict_dump sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [Atom tag ; Atom value] -> `List [ `String tag ; `String value ]
  | List [Atom tag ; List value] ->
     `List [ `String tag ; `String ((List.map flatten_sexp value) |> String.concat ", ") ]
  | List [Atom tag ; x ] -> `List [ `String tag ; `String (": broken2 : " ^ (to_string_hum x)) ]
  | List stuff -> `List [ `String "" ;
                          `String (List.map flatten_sexp stuff |> String.concat "\n") ]
  | x -> `List [ `String "unknown" ; `String ("broken3 : " ^ (to_string_hum x)) ]

let json_of_trace sexp : Yojson.json option =
  let open Sexplib.Sexp in
  let record ~dir ~ty ~bytes = `Assoc [
      "event"     , `String "message"
    ; "direction" , `String dir
    ; "message"   , `String ty
    ; "data"      , match bytes with
                    | Some x -> `List (List.map dict_dump x)
                    | None -> `List [ ]
  ]
  and state ~dir ~ver:(Atom v) ~mach ~rekey =
    `Assoc [
        "event"     , `String "state"
      ; "direction" , `String dir
      ; "version"   , `String v
      (* XXX decode machina into something usable *)
      ; "machina"   , `String (to_string mach)
    ]
  in
  match sexp with
  | List [Atom tag; List sexps] ->
    ( match (tag, sexps) with
      | "handshake-in", [Atom ty; List data ] ->
         Some (record ~dir:"in" ~ty ~bytes:(Some data))
      | "handshake-out", [Atom ty; List data] ->
         Some (record ~dir:"out" ~ty ~bytes:(Some data))
      | "change-cipher-spec-in", _ ->
         Some (record ~dir:"in" ~ty:"ChangeCipherSpec" ~bytes:None)
      | "change-cipher-spec-out", _ ->
         Some (record ~dir:"out" ~ty:"ChangeCipherSpec" ~bytes:None)
      | "application-data-in", bytes ->
         Some (record ~dir:"in" ~ty:"ApplicationData" ~bytes:None)
      | "application-data-out", bytes ->
         Some (record ~dir:"out" ~ty:"ApplicationData" ~bytes:None)
(*      | ("state-in"|"state-out" as dir),
        [ List [ Atom "handshake";
                 List [ List [_; ver] ; List [_; mach]
                      ; List [_; conf] ; List [_; rekey]
                      ; List [_; frag]] ]
               ; _; _; _]
        ->
          let dir = if dir = "state-in" then "in" else "out" in
          Some (state ~dir ~ver ~mach ~rekey) *)
      | _ -> None
    )
  | List [Atom tag; Atom ty] -> (* happens for empty messages: ServerHelloDone / HelloRequest *)
     ( match tag with
      | "handshake-in" ->
         Some (record ~dir:"in" ~ty ~bytes:None)
      | "handshake-out" ->
         Some (record ~dir:"out" ~ty ~bytes:None)
      | _ -> None )
  | _ -> None

let render_traces sexps =
  Yojson.to_string @@ `List (map_partial ~f:json_of_trace sexps)

module Main (C  : CONSOLE)
            (S  : STACKV4)
            (KV : KV_RO) =
struct

  module TLS  = Tls_mirage.Make_flow (S.TCPV4)
  module X509 = Tls_mirage.X509 (KV)
  module Chan = Channel.Make (TLS)
  module Http = HTTP.Make (Chan)

  open Http
  module Body = Cohttp_lwt_body

  let read_kv kv name =
    let file = "web" ^ name in
    KV.size kv file
    >>= function
      | `Error (KV.Unknown_key _) -> fail (Invalid_argument name)
      | `Ok size ->
         KV.read kv file 0 (Int64.to_int size)
         >>= function
           | `Error (KV.Unknown_key _) -> fail (Invalid_argument name)
           | `Ok bufs -> return (Cstruct.copyv bufs)

  let save_traces c sexps =
    (* "saving" traces. Because yeah. *)
    Lwt_list.iter_s (fun sexp ->
      C.log_s c (Sexplib.Sexp.to_string_hum sexp))
      sexps

  let content_type path =
    let open String in
    try
      let idx = String.index path '.' + 1 in
      let rt = String.sub path idx (String.length path - idx) in
      match rt with
      | "js" -> "application/javascript"
      | "css" -> "text/css"
      | "html" -> "text/html"
      | "json" -> "application/json"
      | _ -> "text/plain"
    with _ -> "text/plain"

  let response path =
    Http.Response.make
      ~status:`OK
      ~headers:(Cohttp.Header.of_list [
        "Content-type" , content_type path
      ; "Connection"   , "Keep-Alive"
      ]) ()

  let dispatch (c, kv, _, trace) path =
    let traces = trace () in
(*    lwt _ = save_traces c traces in *)
    let resp = response path in
    try_lwt
      lwt data =
        match path with
        | "/diagram.json" -> return (render_traces traces)
        | "/diagram.txt"  -> return (String.concat "\n" (List.map Sexplib.Sexp.to_string_hum traces))
        | s               -> read_kv kv s
      in
      return (resp, (Body.of_string data))
    with _ ->
      return (Http.Response.make ~status:`Internal_server_error (),
              Body.of_string "<html><head>Server Error</head></html>")

  let handle (_, kv, tls, _ as ctx) conn req body =
    let path = Uri.path req.Http.Request.uri in
    match path with
    | "/rekey"        -> TLS.rekey tls >> return (response "/rekey.txt",
                                                  Body.of_string "intentionally left blank")
    | "/"             -> dispatch ctx "/index.html"
    | s               -> dispatch ctx s

  let upgrade c conf kv tcp =
    let trace, get_trace = make_tracer () in
    TLS.server_of_tcp_flow ~trace conf tcp >>= function
      | `Error _ -> fail (Failure "tls init")
      | `Ok tls  ->
          let ctx = (c, kv, tls, get_trace) in
          let open Http.Server in
          listen { callback = handle ctx; conn_closed = fun _ () -> () } tls

  let start c stack kv =
    lwt cert = X509.certificate kv `Default in
    let conf = Tls.Config.server_exn ~certificate:cert () in
    S.listen_tcpv4 stack 4433 (upgrade c conf kv) ;
    S.listen stack

end
