open Mirage

let data_dir = "data"

let disk  = crunch data_dir

let port =
  let doc = Key.Arg.info ~doc:"Listening port." ["port"] in
  Key.(create "port" Arg.(opt int 443 doc))

let cert =
  let doc = Key.Arg.info ~doc:"path to certificates below data (default: tls/server)"
      ["cert"] in
  Key.(create "cert" Arg.(opt string "tls/server" doc))

let stack =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~arp:farp default_network)

let server =
  let keys = Key.([ abstract port ; abstract cert ]) in
  foreign ~deps:[abstract nocrypto] ~keys "Unikernel.Main" @@
  pclock @-> stackv4 @-> kv_ro @-> job

let () =
  let packages = [
    package "cohttp-mirage" ;
    package "yojson" ;
    package "sexplib" ;
    package ~sublibs:["mirage"] "tls" ;
    package ~sublibs:["lwt"] "logs" ;
  ]
  in
  register ~packages "tls-server" [ server $ default_posix_clock $ stack $ disk ]
