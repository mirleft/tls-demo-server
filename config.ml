open Mirage

let data_dir = "data"

let disk  = direct_kv_ro data_dir
and stack = socket_stackv4 default_console [Ipaddr.V4.any]

let server = foreign "Unikernel.Main" @@ console @-> stackv4 @-> entropy @-> kv_ro @-> job

let () =
  add_to_opam_packages [
    "mirage-clock-unix" ;
    "tls" ;
    "irmin" ;
    "tcpip" ;
    "mirage-http" ;
    "yojson" ;
  ] ;
  add_to_ocamlfind_libraries [
    "mirage-clock-unix" ;
    "tls"; "tls.mirage";
    "irmin.unix" ;
    "tcpip.channel";
    "cohttp.lwt-core"; "mirage-http" ;
    "yojson" ;
  ] ;
  register "tls-server" [ server $ default_console $ stack $ default_entropy $ disk ]
