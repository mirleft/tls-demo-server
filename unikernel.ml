open Mirage_types_lwt
open Lwt.Infix

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
      Bytes.to_string s

  let maybe_add pre post =
    pre ^ (if String.length post > 0 then ": " ^ post else "")

  let exts_dump = function
    | List [Atom "UnknownExtension" ; List [ Atom n ; Atom  value] ] ->
       maybe_add ("Extension " ^ n) (to_hex value)
    | List [Atom tag ; sexps] ->
       maybe_add tag (flatten_sexp ", " sexps)
    | sexp -> to_string_hum sexp

  let dict_dump = function
    | List [Atom "client_random" ; Atom value] ->
       `List [ `String "client_random" ; `String (to_hex value) ]
    | List [Atom "server_random" ; Atom value] ->
       `List [ `String "server_random" ; `String (to_hex value) ]
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

module Trace_session = struct
  type t = {
    mutable jsons : Yojson.json list ;
  }

  let create () = {
    jsons = [] ;
  }

  let trace t sexp =
    ( match Traces_out.json_of_trace sexp with
      | None      -> ()
      | Some json -> t.jsons <- json :: t.jsons )

  let render_traces t =
    let jsons = List.rev t.jsons in
    t.jsons <- [] ;
    Traces_out.render jsons
end


module Main (R : RANDOM) (P : PCLOCK) (T : TIME) (S : STACKV4) (KV : KV_RO) = struct
  module D = Dns_mirage_certify.Make(R)(P)(T)(S)

  module TLS  = Tls_mirage.Make (S.TCPV4)

  module Http = Cohttp_mirage.Server (TLS)

  module Body = Cohttp_lwt.Body

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
                   | "/raphael-min.js" -> "/raphael-min.js"
                   | _ -> "/index.html" )
    in
    KV.size kv file >>= function
    | Error e ->
      Logs_lwt.warn (fun m -> m "failed size of %s: %a" file KV.pp_error e) >>= fun () ->
      Lwt.fail (Invalid_argument name)
    | Ok size ->
      KV.read kv file 0L size >>= function
      | Error e ->
        Logs_lwt.warn (fun m -> m "failed read of %s: %a" file KV.pp_error e) >>= fun () ->
        Lwt.fail (Invalid_argument name)
      | Ok bufs -> Lwt.return (Cstruct.concat bufs)

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
    Cohttp.Response.make
      ~status:`OK
      ~headers:(Cohttp.Header.of_list [
        "Content-type" , content_type path
      ; "Connection"   , "Keep-Alive"
      ]) ()

  let log_request ip port request response =
    let open Cohttp in
    let sget k = match Header.get request.Request.headers k with
      | None -> "-"
      | Some x -> x
    in
    Logs_lwt.info (fun m ->
        m "%a:%d \"%s %s %s\" %d \"%s\" \"%s\""
          Ipaddr.V4.pp ip port
          (Code.string_of_method request.Request.meth)
          request.Request.resource
          (Code.string_of_version request.Request.version)
          (Code.code_of_status response.Response.status)
          (sget "Referer")
          (sget "User-Agent"))

  let dispatch (ip, port, kv, tracer, _) path =
    Lwt.catch (fun () ->
        let resp = response path in
        (match path with
         | "/diagram.json" -> Lwt.return (Trace_session.render_traces tracer)
         | s               -> read_kv kv s >|= Cstruct.to_string) >|= fun data ->
        (resp, (Body.of_string data)))
      (fun _ ->
         Lwt.return (Cohttp.Response.make ~status:`Internal_server_error (),
                     Body.of_string "<html><head>Server Error</head></html>"))

  let handle (ip, port, _, _, tls as ctx) conn req body =
    let path = req.Cohttp.Request.resource in
    (match path with
     | "/rekey" ->
       (TLS.reneg tls >>= function
         | Ok () -> dispatch ctx "/diagram.json"
         | Error e ->
           Logs_lwt.warn (fun m -> m "%a:%d failed renegotation %a" Ipaddr.V4.pp ip port TLS.pp_write_error e) >|= fun () ->
           (Cohttp.Response.make ~status:`Internal_server_error (), Body.of_string "<html><head>Server Error</head></html>"))
     | "/"      -> dispatch ctx "/index.html"
     | s        -> dispatch ctx s) >>= fun (res, body) ->
    log_request ip port req res >|= fun () ->
    (res, body)


  let tls_epoch_to_line epoch =
    let open Tls in
    let version = epoch.Core.protocol_version
    and cipher = epoch.Core.ciphersuite
    in
    Sexplib.Sexp.(to_string_hum (List [
        Core.sexp_of_tls_version version ;
        Ciphersuite.sexp_of_ciphersuite cipher ]))

  let upgrade conf kv tcp =
    let tracer = Trace_session.create () in
    let ip, port = S.TCPV4.dst tcp in
    TLS.server_of_flow ~trace:(Trace_session.trace tracer) conf tcp >>= function
    | Error e ->
      Logs_lwt.warn (fun m -> m "%a:%d failed TLS handshake %a" Ipaddr.V4.pp ip port TLS.pp_write_error e)
    | Ok tls  ->
      (match TLS.epoch tls with
       | Ok epoch -> Logs_lwt.info (fun m -> m "%a:%d established TLS %s" Ipaddr.V4.pp ip port (tls_epoch_to_line epoch))
       | Error () -> Lwt.return_unit) >>= fun () ->
      let ctx = (ip, port, kv, tracer, tls) in
      let thing = Http.make ~callback:(handle ctx) ~conn_closed:(fun _ -> ()) () in
      Http.listen thing tls

  let start _ pclock _time stack kv _ =
    D.retrieve_certificate stack pclock ~dns_key:(Key_gen.dns_key ())
      ~hostname:(Domain_name.of_string_exn (Key_gen.hostname ())) ~key_seed:(Key_gen.key_seed ())
      (Key_gen.dns_server ()) (Key_gen.dns_port ()) >>= fun own_cert ->
    let config = Tls.Config.server ~certificates:own_cert () in
    let port = Key_gen.port () in
    Logs.info (fun m -> m "now starting up, listening on %d" port) ;
    S.listen_tcpv4 stack port (upgrade config kv) ;
    S.listen stack
end
