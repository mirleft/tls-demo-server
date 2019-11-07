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


module Main (C : Mirage_console.S) (R : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (P : Mirage_clock.PCLOCK) (S : Mirage_stack.V4) (KV : Mirage_kv.RO) (Management : Mirage_stack.V4) = struct
  module TLS  = Tls_mirage.Make (S.TCPV4)

  module Http = Cohttp_mirage.Server (TLS)

  module Body = Cohttp_lwt.Body

  let requested_path = function
    | "/diagram.json" -> "/diagram.json"
    | "/gui.js" -> "/gui.js"
    | "/sequence-diagram-min.js" -> "/sequence-diagram-min.js"
    | "/index.html" -> "/index.html"
    | "/style.css" -> "/style.css"
    | "/html5.js" -> "/html5.js"
    | "/jquery-1.11.1.min.js" -> "/jquery-1.11.1.min.js"
    | "/underscore-min.js" -> "/underscore-min.js"
    | "/raphael-min.js" -> "/raphael-min.js"
    | _ -> "/index.html"

  let read_kv kv name =
    KV.get kv (Mirage_kv.Key.v name) >>= function
    | Error e ->
      Logs.warn (fun m -> m "failed get of %s: %a" name KV.pp_error e);
      Lwt.return ""
    | Ok data -> Lwt.return data

  let content_type path =
    match Filename.extension path with
    | ".js" -> "application/javascript"
    | ".css" -> "text/css"
    | ".html" -> "text/html"
    | ".json" -> "application/json"
    | _ -> "text/plain"

  let response path =
    Cohttp.Response.make
      ~status:`OK
      ~headers:(Cohttp.Header.of_list [
        "Content-type" , content_type path
      ; "Connection"   , "Keep-Alive"
      ]) ()

  let create ~f =
    let data : (string, int) Hashtbl.t = Hashtbl.create 7 in
    (fun x ->
       let key = f x in
       let cur = match Hashtbl.find_opt data key with
         | None -> 0
         | Some x -> x
       in
       Hashtbl.replace data key (succ cur)),
    (fun () ->
       let data, total =
         Hashtbl.fold (fun key value (acc, total) ->
             (Metrics.uint key value :: acc), value + total)
           data ([], 0)
       in
       Metrics.uint "total" total :: data)

  let counter_metrics ~f name =
    let open Metrics in
    let doc = "Counter metrics" in
    let incr, get = create ~f in
    let data thing = incr thing; Data.v (get ()) in
    Src.v ~doc ~tags:Metrics.Tags.[] ~data name

  let http_status =
    let f code = Cohttp.Code.code_of_status code |> string_of_int in
    counter_metrics ~f "http_response"
  let http_uri = counter_metrics ~f:(fun x -> x) "http_uri"

  let dispatch kv tracer path =
    let path' = requested_path path in
    let resp = response path' in
    (match path' with
     | "/diagram.json" -> Lwt.return (Trace_session.render_traces tracer)
     | s               -> read_kv kv s) >|= fun data ->
    resp, Body.of_string data

  let access kind =
    let s = ref (0, 0) in
    let open Metrics in
    let doc = "connection statistics" in
    let data () =
      Data.v [
        int "active" (fst !s) ;
        int "total" (snd !s) ;
      ] in
    let tags = Tags.string "kind" in
    let src = Src.v ~doc ~tags:Tags.[ tags ] ~data "connections" in
    (fun action ->
       (match action with
        | `Open -> s := (succ (fst !s), succ (snd !s))
        | `Close -> s := (pred (fst !s), snd !s));
       Metrics.add src (fun x -> x kind) (fun d -> d ()))

  let rekey_access = access "tls-rekey"

  let handle kv tracer tls _conn req _body =
    let path = req.Cohttp.Request.resource in
    (match path with
     | "/rekey" ->
       rekey_access `Open;
       (TLS.reneg tls >>= function
         | Ok () -> rekey_access `Close ; dispatch kv tracer "/diagram.json"
         | Error e ->
           rekey_access `Close;
           Logs.warn (fun m -> m "failed renegotation %a" TLS.pp_write_error e);
           Lwt.return (Cohttp.Response.make ~status:`Internal_server_error (),
                       Body.of_string "<html><head>Server Error</head></html>"))
     | s -> dispatch kv tracer s) >|= fun (res, body) ->
    Metrics.add http_status (fun x -> x) (fun d -> d res.Cohttp.Response.status);
    Metrics.add http_uri (fun x -> x) (fun d -> d path);
    (res, body)

  let tcp_access = access "tcp"
  let tls_access = access "tls"

  let upgrade conf kv tcp =
    tcp_access `Open;
    let tracer = Trace_session.create () in
    TLS.server_of_flow ~trace:(Trace_session.trace tracer) conf tcp >>= function
    | Error e ->
      tcp_access `Close;
      Logs_lwt.warn (fun m -> m "failed TLS handshake %a" TLS.pp_write_error e)
    | Ok tls  ->
      tls_access `Open;
      let thing =
        Http.make ~callback:(handle kv tracer tls)
          ~conn_closed:(fun _ -> tcp_access `Close ; tls_access `Close) ()
      in
      Http.listen thing tls

  module D = Dns_certify_mirage.Make(R)(P)(T)(S)

  module Monitoring = Monitoring_experiments.Make(T)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(P)(Management)

  let start c _random _time _mclock _pclock stack kv management _ info =
    let hostname = Key_gen.name ()
    and syslog = Key_gen.syslog ()
    and monitor = Key_gen.monitor ()
    in
    if Ipaddr.V4.compare syslog Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
    else
      Logs.set_reporter (Syslog.create c management syslog ~hostname ());
    if Ipaddr.V4.compare monitor Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
    else
      Monitoring.create ~hostname monitor management;
    List.iter (fun (p, v) -> Logs.app (fun m -> m "used package: %s %s" p v))
      info.Mirage_info.packages;
    let hostname = Domain_name.(hostname |> of_string_exn |> host_exn) in
    D.retrieve_certificate ~ca:`Production
      stack ~dns_key:(Key_gen.dns_key ())
      ~hostname ~key_seed:(Key_gen.key_seed ())
      (Key_gen.dns_server ()) (Key_gen.dns_port ()) >>= function
    | Error (`Msg m) -> Lwt.fail_with m
    | Ok own_cert ->
      let config = Tls.Config.server ~reneg:true ~certificates:own_cert () in
      let port = Key_gen.port () in
      Logs.info (fun m -> m "now starting up, listening on %d" port) ;
      S.listen_tcpv4 stack ~port (upgrade config kv) ;
      S.listen stack
end
