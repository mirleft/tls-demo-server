
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
  open Irmin_unix

  module Contents_sexp : IrminContents.S with type t = Sexp.t = struct

    module Ident = IrminIdent.Make ( struct
      type t = Sexp.t
      let t_of_sexp s = s
      and sexp_of_t s = s
      let compare = compare
    end )
    include Ident

    let merge = IrminMerge.default (module Ident)
  end

  module Git = IrminGit.FS ( struct
    let root = Some "./trace"
    let bare = true
  end )

  module Store = Git.Make (IrminKey.SHA1) (Contents_sexp) (IrminTag.String)

  let create = Store.create

  let interesting sexp =
    let open Sexp in
    match sexp with
    | List (Atom "application-data-out" ::_) -> false
    | _                                      -> true

  let save t id sexps =
    let ts   = Printf.sprintf "%.05f" (Unix.gettimeofday ()) in
    let sexp = Sexp.List sexps in
    Store.update t [ id ; ts ] sexp

end

module Trace_session = struct

  type t = {
    id    : string ;
    store : Traces_store.Store.t ;
    mutable jsons : Yojson.json list ;
    mutable sexps : Sexplib.Sexp.t list
  }

  let random_tag () =
    Printf.sprintf "%016Lx" @@
      Cstruct.BE.get_uint64 (Nocrypto.Rng.generate 8) 0

  let create store = {
    id    = random_tag () ;
    store = store ;
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
    Traces_store.save t.store t.id sexps

  let render_traces t =
    let jsons = List.rev t.jsons in
    t.jsons <- [] ;
    Traces_out.render jsons

end


module Main (C  : CONSOLE)
            (S  : STACKV4)
            (KV : KV_RO) =
struct

  module TLS  = Tls_mirage.Make_flow (S.TCPV4)
  module X509 = Tls_mirage.X509 (KV) (Clock)
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
      return (Http.Response.make ~status:`Internal_server_error (),
              Body.of_string "<html><head>Server Error</head></html>")

  let handle (_, _, _, tls as ctx) conn req body =
    let path = Uri.path req.Http.Request.uri in
    match path with
    | "/rekey" ->
        (TLS.reneg tls >>= function
          | `Ok -> dispatch ctx "/diagram.json"
          | `Eof -> fail (Failure "EOF while renegotiating")
          | `Error _ -> fail (Failure "error while renegotiating") )
    | "/"      -> dispatch ctx "/index.html"
    | s        -> dispatch ctx s

  let upgrade c irmin conf kv tcp =
    let tracer = Trace_session.create irmin in
    TLS.server_of_tcp_flow ~trace:(Trace_session.trace tracer) conf tcp >>= function
      | `Error _ ->
          Trace_session.flush tracer >> fail (Failure "tls init")
      | `Ok tls  ->
          let ctx = (c, kv, tracer, tls) in
          let open Http.Server in
          listen { callback = handle ctx; conn_closed = fun _ () -> () } tls

  let port = try int_of_string Sys.argv.(1) with _ -> 4433
  let cert = try `Name Sys.argv.(2) with _ -> `Default

  let start c stack kv =
    lwt cert  = X509.certificate kv cert in
    let conf  = Tls.Config.server_exn ~certificate:cert () in
    lwt irmin = Traces_store.create () in
    S.listen_tcpv4 stack port (upgrade c irmin conf kv) ;
    S.listen stack

end
