
open Lwt
open V1_LWT


module Traces_out = struct

  open Sexplib.Sexp

  let rec flatten_sexp comb = function
    | Atom x  -> x
    | List xs -> List.map (flatten_sexp " ") xs |> String.concat comb

  let app_data_to_string bytes =
    [ `List [ `String "" ; `String bytes]]

  let to_hex bytes =
    let c_to_h c idx s =
      let v_to_h = function
        | x when x < 10 -> char_of_int (x + 48)
        | x -> char_of_int (x + 55)
      in
      let i = int_of_char c in
      let high = (0xf0 land i) lsr 4
      and low = 0x0f land i
      in
      Bytes.set s idx (v_to_h high) ;
      Bytes.set s (succ idx) (v_to_h low)
    in
    if String.length bytes = 0 then
      ""
    else
      let s = Bytes.make (String.length bytes * 3 - 1) ' ' in
      for i = 0 to pred (String.length bytes) do
        c_to_h (String.get bytes i) (i * 3) s
      done ;
      s

  let maybe_add pre post =
    pre ^ (if String.length post > 0 then ": " ^ post else "")

  let exts_dump = function
    | List [Atom "UnknownExtension" ; List [ Atom n ; Atom  value] ] ->
       maybe_add ("Extension " ^ n) (to_hex value)
    | List [Atom tag ; sexps] ->
       maybe_add tag (flatten_sexp ", " sexps)
    | sexp -> to_string_hum sexp

  let dict_dump = function
    | List [Atom "random" ; Atom value] ->
       `List [ `String "random" ; `String (to_hex value) ]
    | List [Atom "sessionid" ; List [Atom value]] ->
       `List [ `String "sessionid" ; `String (to_hex value) ]
    | List [Atom "sessionid" ; List []] ->
       `List [ `String "sessionid" ; `String "" ]
    | List [Atom "extensions" ; List exts] ->
       let exts = String.concat ";\n  " (List.map exts_dump exts) in
       `List [ `String "extensions" ; `String exts ]
    | List [Atom tag ; sexps] ->
       `List [ `String tag ; `String (flatten_sexp ", " sexps) ]
    | sexp -> `List [ `String "unknown" ; `String (to_string_hum sexp) ]

  let sexp_to_hex = function
    | Atom x -> `List [ `String "" ; `String (to_hex x)]
    | List _ -> `List [ `String "" ; `String "cannot sexp_to_hex of a list" ]

  let json_of_trace sexp : Yojson.json option =
    let record ~dir ~ty ~bytes = `Assoc [
        "event"     , `String "message"
      ; "direction" , `String dir
      ; "message"   , `String ty
      ; "data"      , `List bytes
    ]
    and note ~msg ~data = `Assoc [
        "event"     , `String "note"
      ; "message"   , `String msg
      ; "data"      , `String data
    ]
    and app_data_out_subst =
      [ `List [`String ""; `String "(data on this page)" ]]
    in
    match sexp with
    | List [Atom tag; sexps] ->
      ( match tag, sexps with
        | "handshake-in", List [Atom "ClientHello"; List data ] ->
           Some (record ~dir:"in" ~ty:"ClientHello" ~bytes:(List.map dict_dump data))
        | "handshake-out", List [Atom "ServerHello"; List data] ->
           Some (record ~dir:"out" ~ty:"ServerHello" ~bytes:(List.map dict_dump data))
        | "handshake-out", List [Atom "Certificate"; List data] ->
           Some (record ~dir:"out" ~ty:"Certificate" ~bytes:(List.map sexp_to_hex data))
        | "handshake-in", List [ Atom ty ; Atom data ] ->
           let data = to_hex data in
           Some (record ~dir:"in" ~ty ~bytes:[`List [`String "" ; `String data]])
        | "handshake-out", List [ Atom ty ; Atom data ] ->
           let data = to_hex data in
           Some (record ~dir:"out" ~ty ~bytes:[`List [`String "" ; `String data]])
        | "handshake-in", Atom ty ->
           Some (record ~dir:"in" ~ty ~bytes:[])
        | "handshake-out", Atom ty ->
           Some (record ~dir:"out" ~ty ~bytes:[])
        | "change-cipher-spec-in", _ ->
           Some (record ~dir:"in" ~ty:"ChangeCipherSpec" ~bytes:[])
        | "change-cipher-spec-out", _ ->
           Some (record ~dir:"out" ~ty:"ChangeCipherSpec" ~bytes:[])
        | "application-data-in", Atom bytes ->
           Some (record ~dir:"in" ~ty:"ApplicationData" ~bytes:(app_data_to_string bytes))
        | "application-data-out", Atom _ ->
           Some (record ~dir:"out" ~ty:"ApplicationData" ~bytes:app_data_out_subst)
        | "master-secret", Atom bytes ->
           let ms =
             let parts =
               String.([ sub bytes 0 8 ; sub bytes 8 8 ; sub bytes 16 8 ;
                         sub bytes 24 8 ; sub bytes 32 8 ; sub bytes 40 8 ])
             in
             let hex = List.map to_hex parts in
             String.concat "\n" hex
           in
           let msg = "Master secret:\n" ^ ms in
           Some (note ~msg:msg ~data:"")
        | "cipher", List [ Atom kex ; papr ] ->
           let pp = "KEX: " ^ kex ^ "\n" ^ (flatten_sexp " " papr) in
           Some (note ~msg:pp ~data:"")
        | "version", Atom ty ->
           Some (note ~msg:ty ~data:"")
        | _ -> None
       )
    | _ -> None

  let render jsons = Yojson.to_string (`List jsons)

end

module Traces_store = struct
  open Sexplib
  let root = "./trace"

  let interesting sexp =
    let open Sexp in
    match sexp with
    | List (Atom "application-data-out" ::_) -> false
    | _                                      -> true

  let save id sexps =
    let ts = Printf.sprintf "%.05f" (Unix.gettimeofday ()) in
    let dir = Filename.concat root id in
    (if not (Sys.file_exists root && Sys.is_directory root) then
       Lwt_unix.mkdir root 0o755
     else
       Lwt.return_unit) >>= fun () ->
    (if not (Sys.file_exists dir && Sys.is_directory dir) then
       Lwt_unix.mkdir dir 0o755
     else
       Lwt.return_unit) >>= fun () ->
    let file = Filename.concat dir ts in
    (if Sys.file_exists file then
       Lwt_unix.openfile file [Unix.O_WRONLY ; Unix.O_APPEND] 0o644
     else
       Lwt_unix.openfile file [Unix.O_WRONLY ; Unix.O_CREAT] 0o644) >>= fun fd ->
    let str = String.concat "\n" (List.map Sexp.to_string_hum sexps) in
    Lwt_unix.write fd str 0 (String.length str) >>= fun _ ->
    Lwt_unix.close fd
end

module Trace_session = struct

  type t = {
    id    : string ;
    mutable jsons : Yojson.json list ;
    mutable sexps : Sexplib.Sexp.t list
  }

  let random_tag () =
    Printf.sprintf "%016Lx" @@
      Cstruct.BE.get_uint64 (Nocrypto.Rng.generate 8) 0

  let create () = {
    id    = random_tag () ;
    jsons = [] ;
    sexps = []
  }

  let trace t sexp =
    ( match Traces_out.json_of_trace sexp with
      | None      -> ()
      | Some json -> t.jsons <- json :: t.jsons ) ;
    ( if Traces_store.interesting sexp then
      t.sexps <- sexp :: t.sexps )

  let flush t =
    let sexps = List.rev t.sexps in
    t.sexps <- [] ;
    Traces_store.save t.id sexps

  let render_traces t =
    let jsons = List.rev t.jsons in
    t.jsons <- [] ;
    Traces_out.render jsons

end


module Main (C  : CONSOLE)
            (S  : STACKV4)
            (KV : KV_RO) =
struct

  module TLS  = Tls_mirage.Make (S.TCPV4)
  module X509 = Tls_mirage.X509 (KV) (Clock)
  module Http = Cohttp_mirage.Server (TLS)

  module Body = Cohttp_lwt_body

  let read_kv kv name =
    let file = "web" ^
                 ( match name with
                   | "/gui.js" -> "/gui.js"
                   | "/sequence-diagram-min.js" -> "/sequence-diagram-min.js"
                   | "/index.html" -> "/index.html"
                   | "/style.css" -> "/style.css"
                   | "/html5.js" -> "/html5.js"
                   | "/jquery-1.11.1.min.js" -> "/jquery-1.11.1.min.js"
                   | "/underscore-min.js" -> "/underscore-min.js"
                   | "/raphael-min.js" -> "/raphael-min.js" )
    in
    KV.size kv file
    >>= function
      | `Error (KV.Unknown_key _) -> fail (Invalid_argument name)
      | `Ok size ->
         KV.read kv file 0 (Int64.to_int size)
         >>= function
           | `Error (KV.Unknown_key _) -> fail (Invalid_argument name)
           | `Ok bufs -> return (Cstruct.copyv bufs)

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
    Cohttp_lwt.Response.make
      ~status:`OK
      ~headers:(Cohttp.Header.of_list [
        "Content-type" , content_type path
      ; "Connection"   , "Keep-Alive"
      ]) ()

  let dispatch (c, kv, tracer, _) path =
    lwt () = Trace_session.flush tracer in
    let resp   = response path in
    try_lwt
      lwt data =
        match path with
        | "/diagram.json" -> return (Trace_session.render_traces tracer)
        | s               -> read_kv kv s
      in
      return (resp, (Body.of_string data))
    with _ ->
      return (Cohttp_lwt.Response.make ~status:`Internal_server_error (),
              Body.of_string "<html><head>Server Error</head></html>")

  let handle (_, _, _, tls as ctx) conn req body =
    let path = Uri.path req.Cohttp_lwt.Request.uri in
    match path with
    | "/rekey" ->
        (TLS.reneg tls >>= function
          | `Ok () -> dispatch ctx "/diagram.json"
          | `Eof -> fail (Failure "EOF while renegotiating")
          | `Error _ -> fail (Failure "error while renegotiating") )
    | "/"      -> dispatch ctx "/index.html"
    | s        -> dispatch ctx s

  let upgrade c conf kv tcp =
    let tracer = Trace_session.create () in
    TLS.server_of_flow ~trace:(Trace_session.trace tracer) conf tcp >>= function
      | `Error _ | `Eof ->
          Trace_session.flush tracer >> fail (Failure "tls init")
      | `Ok tls  ->
         let ctx = (c, kv, tracer, tls) in
         let thing = Http.make ~callback:(handle ctx) ~conn_closed:(fun _ -> ()) () in
         Http.listen thing tls

  let port = try int_of_string Sys.argv.(1) with _ -> 4433
  let cert = try `Name Sys.argv.(2) with _ -> (`Name "tls/server")

  let start c stack kv =
    lwt cert  = X509.certificate kv cert in
    let conf  = Tls.Config.server ~certificates:(`Single cert) ~reneg:true () in
    S.listen_tcpv4 stack port (upgrade c conf kv) ;
    S.listen stack

end
