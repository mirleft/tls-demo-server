
open Lwt
open V1_LWT


module Traces_out = struct

  open Sexplib.Sexp

  let bytes_dump = function
    | List bs ->
        let repr =
          List.map (fun (List bytes) ->
            `String (List.map (fun (Atom x) -> x) bytes |> String.concat " "))
          bs in
        `List repr

  let rec flatten_sexp = function
    | Atom x  -> x
    | List xs -> List.map flatten_sexp xs |> String.concat " "

  let rec implode str =
    let rec esc = function
      | []             -> [""]
      | '\r'::'\n'::xs
      | '\n'::xs       -> "\n" :: esc xs
      | x::xs          -> Char.escaped x :: esc xs in
    String.concat "" (esc str)

  let hex_sexp_to_string xs =
    let to_byte str =
      char_of_int (Scanf.sscanf str "%x" (fun x -> x)) in
    xs |> List.map (fun (List bs) -> List.map (fun (Atom str) -> to_byte str) bs)
       |> List.concat
       |> implode

  let app_data_to_string bytes =
    [ `List [ `String "" ; `String (hex_sexp_to_string bytes)]]

  let dict_dump = function
    | List [Atom tag ; Atom value] -> `List [ `String tag ; `String value ]
    | List [Atom tag ; List value] ->
      `List [ `String tag ; `String ((List.map flatten_sexp value) |> String.concat ", ") ]
    | List [Atom tag ; x ] -> `List [ `String tag ; `String (": broken2 : " ^ (to_string_hum x)) ]
    | List stuff -> `List [ `String "" ;
                            `String (List.map flatten_sexp stuff |> String.concat " ") ]
    | x -> `List [ `String "unknown" ; `String ("broken3 : " ^ (to_string_hum x)) ]

  let split_string s =
    let size = String.length s in
    let mid = size / 2 in
    String.([sub s 0 mid ; sub s (size - mid) mid])

  let pp_cipher = function
    | [ kex ; enc ; hash ] -> "KEX: " ^ kex ^ "\nENC: " ^ enc ^ "\nHASH: " ^ hash
    | _                    -> "FAIL"

  let json_of_trace sexp : Yojson.json option =
    let record ~dir ~ty ~bytes = `Assoc [
        "event"     , `String "message"
      ; "direction" , `String dir
      ; "message"   , `String ty
      ; "data"      , `List bytes
    ]
    and state ~dir ~ver:(Atom v) ~mach ~rekey =
      `Assoc [
          "event"     , `String "state"
        ; "direction" , `String dir
        ; "version"   , `String v
        (* XXX decode machina into something usable *)
        ; "machina"   , `String (to_string mach)
      ]
    and note ~msg ~data =
      `Assoc [
          "event"     , `String "note"
        ; "message"   , `String msg
        ; "data"      , `String data
      ]
    in
    let app_data_out_subst =
      [ `List [`String ""; `String "(data on this page)" ]] in
    match sexp with
    | List [Atom tag; List sexps] ->
      ( match (tag, sexps) with
        | "handshake-in", [Atom ty; List data ] ->
           Some (record ~dir:"in" ~ty ~bytes:(List.map dict_dump data))
        | "handshake-out", [Atom ty; List data] ->
           Some (record ~dir:"out" ~ty ~bytes:(List.map dict_dump data))
        | "change-cipher-spec-in", _ ->
           Some (record ~dir:"in" ~ty:"ChangeCipherSpec" ~bytes:[])
        | "change-cipher-spec-out", _ ->
           Some (record ~dir:"out" ~ty:"ChangeCipherSpec" ~bytes:[])
        | "application-data-in", bytes ->
           Some (record ~dir:"in" ~ty:"ApplicationData" ~bytes:(app_data_to_string bytes))
        | "application-data-out", bytes ->
           Some (record ~dir:"out" ~ty:"ApplicationData" ~bytes:app_data_out_subst)
        | "master-secret", bytes ->
           let ms = List.map flatten_sexp bytes |>
                      List.map split_string |>
                      List.flatten |>
                      String.concat "\n"
           in
           let msg = "Master secret:\n" ^ ms in
           Some (note ~msg:msg ~data:"")
        | "cipher", parts ->
           let pp = List.map flatten_sexp parts |> pp_cipher in
           Some (note ~msg:pp ~data:"")
        | _ -> None
      )
    | List [Atom tag; Atom ty] -> (* happens for empty messages: ServerHelloDone / HelloRequest *)
      ( match tag with
        | "handshake-in" ->
           Some (record ~dir:"in" ~ty ~bytes:([]))
        | "handshake-out" ->
           Some (record ~dir:"out" ~ty ~bytes:([]))
        | "version" ->
           Some (note ~msg:ty ~data:"")
        | _ -> None )
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
