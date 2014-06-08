
open Lwt
open V1_LWT

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

  let diagram _ =
    let data = "{ foo : 'bar' ; bla : 'baz'  }" in
    data

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

  let dispatch kv path =
    let header =
      let ct = content_type path in
      let h = Cohttp.Header.init_with "Content-type" ct in
      Cohttp.Header.add h "Connection" "Keep-Alive"
    in
    let resp = Http.Response.make ~status:`OK ~headers:header () in
    try_lwt
      lwt data = match path with
                 | "/diagram.json" -> return (diagram ())
                 | s               -> read_kv kv s
      in
      return (resp, (Body.of_string data))
    with _ ->
      return (Http.Response.make ~status:`Internal_server_error (),
              Body.of_string "<html><head>Server Error</head></html>")

  let handle c kv conn req body =
    let path = Uri.path req.Http.Request.uri in
    match path with
    | "/"             -> dispatch kv "/index.html"
    | s               -> dispatch kv s

  let upgrade c conf kv tcp =
    TLS.server_of_tcp_flow conf tcp >>= function
      | `Error _ -> fail (Failure "tls init")
      | `Ok tls  ->
          let open Http.Server in
          listen { callback = handle c kv ; conn_closed = fun _ () -> () } tls

  let start c stack kv =
    lwt cert = X509.certificate kv `Default in
    let conf = Tls.Config.server_exn ~certificate:cert () in
    S.listen_tcpv4 stack 4433 (upgrade c conf kv) ;
    S.listen stack

end
