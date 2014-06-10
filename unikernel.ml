
open Lwt
open V1_LWT

let make_tracer () =
  let traces = ref [] in
  let trace sexp = (traces := sexp :: !traces)
  and get ()     = List.rev !traces in
  (trace, get)

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
    lwt _ = save_traces c traces in
    let resp = response path in
    try_lwt
      lwt data =
        match path with
        | "/diagram.json"
        | "/diagram.txt" -> return (render_traces traces)
        | s              -> read_kv kv s
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
